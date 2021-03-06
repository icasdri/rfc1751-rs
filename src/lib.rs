#[cfg(test)]
extern crate rand;

use std::cmp::Ordering;
use std::ops::Deref;

mod words;
use words::WORDS;

#[derive(Debug)]
enum FromTransformSubkeyError {
    InvalidWord(String),
    IncorrectParity,
    TooShort, // aka. too few words (not enough to pull six)
    NothingToTransform
}

#[derive(Debug)]
pub enum FromRfc1751Error {
    InvalidWord(String),
    IncorrectParity(Vec<u8>), // we still hand back what we got, despite wrong parity
    NotMultipleOfSixWords // not a multiple of six words
}

#[derive(Debug)]
pub enum ToRfc1751Error {
    NotMultipleOfEight
}

pub trait FromRfc1751 {
    fn from_rfc1751(&self) -> Result<Vec<u8>, FromRfc1751Error>;
}

pub trait ToRfc1751 {
    fn to_rfc1751(&self) -> Result<String, ToRfc1751Error>;
}

impl<C, T> FromRfc1751 for C where C: Deref<Target=[T]>, T: AsRef<str> {
    fn from_rfc1751(&self) -> Result<Vec<u8>, FromRfc1751Error> {
        from_rfc1751_transform_general(self.iter())
    }
}

impl FromRfc1751 for str {
    fn from_rfc1751(&self) -> Result<Vec<u8>, FromRfc1751Error> {
        from_rfc1751_transform_general(self.split_whitespace())
    }
}

impl ToRfc1751 for [u8] {
    fn to_rfc1751(&self) -> Result<String, ToRfc1751Error> {
        if self.len() % 8 != 0 {
            return Err(self::ToRfc1751Error::NotMultipleOfEight);
        }

        let mut result = String::new();
        for subkey in self.chunks(8) {
            to_rfc1751_transform_append_subkey(subkey, &mut result);
            result.push(' ');
        }
        // pop trailing space
        result.pop();

        Ok(result)
    }
}


fn get_word_index(word: &str) -> Result<usize, FromTransformSubkeyError> {
    match word.len() {
        // we know that all valid words are 1 to 4 letters long
        1...4 => {
            // binary search for the word's index (aka. bit value)
            WORDS.binary_search_by(|g|
                if g.len() < 4 && word.len() == 4 {
                    Ordering::Less
                } else if g.len() == 4 && word.len() < 4 {
                    Ordering::Greater
                } else {
                    g.cmp(&word)
                }
            ).map_err(|_| FromTransformSubkeyError::InvalidWord(word.to_owned()))
        },
        _ => Err(FromTransformSubkeyError::InvalidWord(word.to_owned()))
    }
}

#[allow(unused_parens)]
fn from_rfc1751_transform_append_subkey<I, T>(iter: &mut I,
        append_to: &mut Vec<u8>) -> Result<(), FromTransformSubkeyError>
        where I: Iterator<Item=T>, T: AsRef<str> {

    let mut count = 0; // counter for ensuring we have enough for six words
                       // (we just use the first six if we have more)
    let mut build: usize = 0;
    let mut have = 0;
    let mut sum_for_parity: usize = 0;
    loop {
        if have > 8 {
            // if we have 8 bits or more available, grab the first 8
            let d = have - 8;
            let mut commit = (build >> d);
            append_to.push(commit as u8);

            build = build % (2 << (d-1));
            have = d;

            // two-bit parity calculation
            while commit > 0 {
                sum_for_parity += (commit % 4);
                commit /= 4;
            }
        } else {
            // don't pull more than six words
            if count == 6 {
                break;
            }

            // otherwise pull another word to get more bits
            let pull_word = match iter.next() {
                Some(w) => {
                    count += 1;
                    w
                },
                None => break
            };
            let addition = try!(get_word_index(pull_word.as_ref()));

            // shift our existing bits to make room for the addition if necessary
            if have > 0 {
                build *= (2 << 10);
            }

            // append the new bits
            build += addition;
            have += 11;
        }
    }

    if count == 0 {
        Err(FromTransformSubkeyError::NothingToTransform)
    } else if count != 6 {
        // check for count (need 6 words)
        Err(FromTransformSubkeyError::TooShort)
    } else if build != (sum_for_parity % 4) {
        // check for parity (the last two bits were left in build)
        Err(FromTransformSubkeyError::IncorrectParity)
    } else {
        Ok(())
    }
}

fn from_rfc1751_transform_general<I, T>(mut iter: I) -> Result<Vec<u8>, FromRfc1751Error>
        where I: Iterator<Item=T>, T:AsRef<str> {
    let mut fill = Vec::new();
    loop {
        let result = from_rfc1751_transform_append_subkey(&mut iter, &mut fill);
        if result.is_err() {
            match result.unwrap_err() {
                FromTransformSubkeyError::NothingToTransform => break,
                FromTransformSubkeyError::InvalidWord(w) =>
                    return Err(FromRfc1751Error::InvalidWord(w)),
                FromTransformSubkeyError::TooShort =>
                    return Err(FromRfc1751Error::NotMultipleOfSixWords),
                FromTransformSubkeyError::IncorrectParity =>
                    return Err(FromRfc1751Error::IncorrectParity(fill))
            }
        }
    }
    Ok(fill)
}

#[allow(unused_parens)]
fn to_rfc1751_transform_append_subkey(input: &[u8], append_to: &mut String) {
    let mut build: usize = 0;
    let mut have = 0;
    let mut sum_for_parity = 0;
    let mut iter = input.iter();
    loop {
        if have > 11 {
            let d = have - 11;
            let commit = (build >> d);
            append_to.push_str(WORDS[commit]);
            append_to.push(' ');

            build = build % (2 << (d-1));
            have = d
        } else {
            let mut addition = match iter.next() {
                Some(&x) => x as usize,
                None => break
            };

            if have > 0 {
                build *= (2 << 7);
            }

            build += addition;
            have += 8;

            // two-bit parity calculation
            while addition > 0 {
                sum_for_parity += (addition % 4);
                addition /= 4;
            }
        }

    }

    // make room for the two-bit parity
    build *= 4;
    // append the parity
    build += (sum_for_parity % 4);
    // commit the final word (that includes the parity bits)
    append_to.push_str(WORDS[build]);
}


#[cfg(test)]
mod tests {
    macro_rules! parameterized_tests {
        ( $func:path; $( $i:ident: $( $param:expr ),*  => $expected:expr )* ) => {
            $(
                #[test]
                fn $i() {
                    $func($( $param ),*, $expected);
                }
            )*
        }
    }

    use rand::{thread_rng, Rng};
    use words::WORDS;
    use super::FromRfc1751;
    use super::ToRfc1751;

    parameterized_tests! {
        from_subkey_test;

        from_subkey_test_01: &["TIDE", "ITCH", "SLOW", "REIN", "RULE", "MOT"] =>
                             &[0xEB, 0x33, 0xF7, 0x7E, 0xE7, 0x3D, 0x40, 0x53]
        from_subkey_test_02: &["RASH", "BUSH", "MILK", "LOOK", "BAD", "BRIM"] =>
                             &[0xCC, 0xAC, 0x2A, 0xED, 0x59, 0x10, 0x56, 0xBE]
        from_subkey_test_03: &["AVID", "GAFF", "BAIT", "ROT", "POD", "LOVE"] =>
                             &[0x4F, 0x90, 0xFD, 0x44, 0x1C, 0x53, 0x47, 0x66]
        from_subkey_test_04: &["TROD", "MUTE", "TAIL", "WARM", "CHAR", "KONG"] =>
                             &[0xEF, 0xF8, 0x1F, 0x9B, 0xFB, 0xC6, 0x53, 0x50]
        from_subkey_test_05: &["HAAG", "CITY", "BORE", "O", "TEAL", "AWL"] =>
                             &[0x92, 0x0C, 0xDD, 0x74, 0x16, 0xDE, 0x80, 0x09]
    }

    fn from_subkey_test(input: &[&'static str], expected: &[u8]) {
        let mut fill = Vec::new();
        let mut iter = input.iter();
        let result = super::from_rfc1751_transform_append_subkey(&mut iter, &mut fill);
        assert!(result.is_ok());
        assert_eq!(fill, expected);
    }

    #[test]
    fn from_subkey_test_too_short() {
        let mut fill = Vec::new();
        let input = &["TROD", "MUTE", "TAIL", "WARM", "CHAR"]; // only five words
        let mut iter = input.iter();
        let result = super::from_rfc1751_transform_append_subkey(&mut iter, &mut fill);
        assert!(result.is_err());
        assert!(match result.unwrap_err() {
            super::FromTransformSubkeyError::TooShort => true,
            _ => false
        });
    }

    #[test]
    fn from_subkey_test_nothing() {
        let mut fill = Vec::new();
        let input: &[&str] = &[]; // empty!
        let mut iter = input.iter();
        let result = super::from_rfc1751_transform_append_subkey(&mut iter, &mut fill);
        assert!(result.is_err());
        assert!(match result.unwrap_err() {
            super::FromTransformSubkeyError::NothingToTransform => true,
            _ => false
        });
    }

    parameterized_tests! {
        from_test;

        from_test_01: "TIDE ITCH SLOW REIN RULE MOT" =>
                      &[0xEB, 0x33, 0xF7, 0x7E, 0xE7, 0x3D, 0x40, 0x53]
        from_test_02: "RASH BUSH MILK LOOK BAD BRIM AVID GAFF BAIT ROT POD LOVE" =>
                      &[0xCC, 0xAC, 0x2A, 0xED, 0x59, 0x10, 0x56, 0xBE,
                        0x4F, 0x90, 0xFD, 0x44, 0x1C, 0x53, 0x47, 0x66]
        from_test_03: "TROD MUTE TAIL WARM CHAR KONG HAAG CITY BORE O TEAL AWL" =>
                      &[0xEF, 0xF8, 0x1F, 0x9B, 0xFB, 0xC6, 0x53, 0x50,
                        0x92, 0x0C, 0xDD, 0x74, 0x16, 0xDE, 0x80, 0x09]
        from_test_04: concat!(
                        "RASH BUSH MILK LOOK BAD BRIM AVID GAFF BAIT ROT POD LOVE ",
                        "TROD MUTE TAIL WARM CHAR KONG HAAG CITY BORE O TEAL AWL ",
                        "TIDE ITCH SLOW REIN RULE MOT"
                      ) =>
                      &[0xCC, 0xAC, 0x2A, 0xED, 0x59, 0x10, 0x56, 0xBE,
                        0x4F, 0x90, 0xFD, 0x44, 0x1C, 0x53, 0x47, 0x66,
                        0xEF, 0xF8, 0x1F, 0x9B, 0xFB, 0xC6, 0x53, 0x50,
                        0x92, 0x0C, 0xDD, 0x74, 0x16, 0xDE, 0x80, 0x09,
                        0xEB, 0x33, 0xF7, 0x7E, 0xE7, 0x3D, 0x40, 0x53]
    }

    fn from_test(input: &'static str, expected: &[u8]) {
        let result = input.from_rfc1751();
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), expected);

        let owned = input.to_owned();
        let result_owned = owned.from_rfc1751();
        assert!(result_owned.is_ok());
        assert_eq!(result_owned.unwrap(), expected);

        let splitted_owned: Vec<&str> = owned.split_whitespace().collect();
        let result_splitted = splitted_owned.from_rfc1751();
        assert!(result_splitted.is_ok());
        assert_eq!(result_splitted.unwrap(), expected);
    }

    parameterized_tests! {
        to_subkey_test;

        to_subkey_test_01: &[0xEB, 0x33, 0xF7, 0x7E, 0xE7, 0x3D, 0x40, 0x53] =>
                           "TIDE ITCH SLOW REIN RULE MOT"
        to_subkey_test_02: &[0xCC, 0xAC, 0x2A, 0xED, 0x59, 0x10, 0x56, 0xBE] =>
                           "RASH BUSH MILK LOOK BAD BRIM"
        to_subkey_test_03: &[0x4F, 0x90, 0xFD, 0x44, 0x1C, 0x53, 0x47, 0x66] =>
                           "AVID GAFF BAIT ROT POD LOVE"
        to_subkey_test_04: &[0xEF, 0xF8, 0x1F, 0x9B, 0xFB, 0xC6, 0x53, 0x50] =>
                           "TROD MUTE TAIL WARM CHAR KONG"
        to_subkey_test_05: &[0x92, 0x0C, 0xDD, 0x74, 0x16, 0xDE, 0x80, 0x09] =>
                           "HAAG CITY BORE O TEAL AWL"
    }

    fn to_subkey_test(target: &[u8], expected: &'static str) {
        let mut result = String::new();
        super::to_rfc1751_transform_append_subkey(target, &mut result);
        assert_eq!(result, expected);
    }

    parameterized_tests! {
        to_test;

        to_test_01: &[0xEB, 0x33, 0xF7, 0x7E, 0xE7, 0x3D, 0x40, 0x53] =>
                    "TIDE ITCH SLOW REIN RULE MOT"
        to_test_02: &[0xCC, 0xAC, 0x2A, 0xED, 0x59, 0x10, 0x56, 0xBE,
                      0x4F, 0x90, 0xFD, 0x44, 0x1C, 0x53, 0x47, 0x66] =>
                    "RASH BUSH MILK LOOK BAD BRIM AVID GAFF BAIT ROT POD LOVE"
        to_test_03: &[0xEF, 0xF8, 0x1F, 0x9B, 0xFB, 0xC6, 0x53, 0x50,
                      0x92, 0x0C, 0xDD, 0x74, 0x16, 0xDE, 0x80, 0x09] =>
                    "TROD MUTE TAIL WARM CHAR KONG HAAG CITY BORE O TEAL AWL"
        to_test_04: &[0xCC, 0xAC, 0x2A, 0xED, 0x59, 0x10, 0x56, 0xBE,
                      0x4F, 0x90, 0xFD, 0x44, 0x1C, 0x53, 0x47, 0x66,
                      0xEF, 0xF8, 0x1F, 0x9B, 0xFB, 0xC6, 0x53, 0x50,
                      0x92, 0x0C, 0xDD, 0x74, 0x16, 0xDE, 0x80, 0x09,
                      0xEB, 0x33, 0xF7, 0x7E, 0xE7, 0x3D, 0x40, 0x53] =>
                    concat!(
                        "RASH BUSH MILK LOOK BAD BRIM AVID GAFF BAIT ROT POD LOVE ",
                        "TROD MUTE TAIL WARM CHAR KONG HAAG CITY BORE O TEAL AWL ",
                        "TIDE ITCH SLOW REIN RULE MOT"
                    )
    }

    fn to_test(target: &[u8], expected: &'static str) {
        let result = target.to_rfc1751();
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn to_test_wrong_length() {
        // this input has length of 10, not a multiple of eight
        let target = &[0xFF, 0x4F, 0x90, 0xFD, 0x44, 0x1C, 0x53, 0x47, 0x66, 0xFF];
        let result = target.to_rfc1751();
        assert!(result.is_err());
        assert!(match result.unwrap_err() {
            super::ToRfc1751Error::NotMultipleOfEight => true,
            // There's currently only one ToRfc1751Error, so this branch would be unreachable
            // _ => false
        });
    }

    parameterized_tests! {
        word_index_test;

        word_index_test_01: "TIDE" => Ok(1881)
        word_index_test_02: "BAIT" => Ok(648)
        word_index_test_03: "AWL" => Ok(39)
        word_index_test_04: "tide" => Err(()) // we are case-sensitive, clients deal with case
        word_index_test_05: "BABABA" => Err(())
        word_index_test_06: "" => Err(())
        word_index_test_07: "AX" => Ok(41)
    }

    fn word_index_test(word: &'static str, expected: Result<usize, ()>) {
        let result = super::get_word_index(word);
        match expected {
            Ok(x) => {
                assert!(result.is_ok());
                assert_eq!(result.unwrap(), x);
            },
            Err(_) => {
                assert!(result.is_err());
                assert!(match result.unwrap_err() {
                    super::FromTransformSubkeyError::InvalidWord(w) => (word == w),
                    _ => false
                });
            }
        }
    }

    #[test]
    fn word_index_test_ten_random() {
        let mut rng = thread_rng();
        for _ in 0..10 {
            let n: usize = rng.gen_range(0, WORDS.len());
            let word = WORDS[n];
            let result = super::get_word_index(word);
            println!("word_index_test_five_random: case {} <--> {}; result {:?}",
                     n, word, result);
            assert!(result.is_ok());
            assert_eq!(result.unwrap(), n);
        }
    }

}
