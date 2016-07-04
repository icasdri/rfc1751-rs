#[cfg(test)]
extern crate rand;

use std::cmp::Ordering;

mod words;
use words::WORDS;

#[derive(Debug)]
pub enum FromRfc1751Error<'a> {
    InvalidWord(&'a str),
    IncorrectParity(Vec<u8>) // we still hand back what we got, despite wrong parity
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

fn get_word_index(word: &str) -> Result<usize, FromRfc1751Error> {
    match word.len() {
        // we know that all valid words are 1 to 4 letters long
        1...4 => {
            // binary search for the word's index (aka. bit value)
            WORDS.binary_search_by(|g| 
                match g.len().cmp(&(word.len())) {
                    Ordering::Equal => g.cmp(&word),
                    Ordering::Less => Ordering::Less,
                    Ordering::Greater => Ordering::Greater,
                }
            )
                 .map_err(|_| FromRfc1751Error::InvalidWord(word))
        },
        _ => Err(FromRfc1751Error::InvalidWord(word))
    }
}

fn from_rfc1751_transform_append_subkey<I, T>(input: I, append_to: &mut Vec<u8>)
        where I: IntoIterator<Item=T>, T: AsRef<str> {
    for s in input {
        // TODO: testing only
        println!("{}", get_word_index(s.as_ref()).unwrap());
    }
}

impl<I, T> FromRfc1751 for I where I: IntoIterator<Item=T>, T: AsRef<str> {
    fn from_rfc1751(&self) -> Result<Vec<u8>, FromRfc1751Error> {
        // TODO: testing only
        Ok(vec![22])
    }
}

impl FromRfc1751 for AsRef<str> {
    fn from_rfc1751(&self) -> Result<Vec<u8>, FromRfc1751Error> {
        // TODO: testing only
        Ok(vec![22])
    }
}

fn to_rfc1751_transform_append_subkey(input: &[u8], append_to: &mut String) {
    let mut build: usize = 0;
    let mut have = 0;
    let mut sum_for_parity = 0;
    for current in input.iter() {
        have += 8;
        if have > 11 {
            let d = have - 11;
            // shift all bits right by d bits (leaving only the first 11-d bits)
            build += (current >> d) as usize;
            append_to.push_str(WORDS[build]);
            append_to.push(' ');
            // reset and carry over if necessary
            // shift the last d bits left by 11-d bits in eleven-bit space
            build = (*current as usize % (2 << (d-1))) * (2 << ((11-d)-1));
            have = d
        } else {
            let d = 11 - have;
            // shift all bits left by d bits in eleven-bit space
            build += (*current as usize) * (2 << (d-1));
        }

        // two-bit parity calculation
        let mut drain = *current as usize;
        while drain > 0 {
            sum_for_parity += drain % 4;
            drain /= 4;
        }
    }

    append_to.push_str(WORDS[build + (sum_for_parity % 4)]);
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

    #[test]
    fn word_index_test_five_random() {
        let mut rng = thread_rng();
        for _ in 0..5 {
            let n: usize = rng.gen_range(0, WORDS.len());
            let word = WORDS[n];
            let result = super::get_word_index(word);
            assert!(result.is_ok());
            assert_eq!(result.unwrap(), n);
        }
    }

    parameterized_tests! {
        word_index_test;

        word_index_test_01: "TIDE" => Ok(1881)
        word_index_test_02: "BAIT" => Ok(648)
        word_index_test_03: "AWL" => Ok(39)
        word_index_test_04: "tide" => Err(()) // we are case-sensitive, clients deal with case
        word_index_test_05: "BABABA" => Err(())
        word_index_test_06: "" => Err(())
    }

    fn word_index_test(word: &'static str, expected: Result<usize, ()>) {
        let result = super::get_word_index(word);
        println!("{:?}", result);
        println!("{}", WORDS[649]);
        match expected {
            Ok(x) => {
                assert!(result.is_ok());
                assert_eq!(result.unwrap(), x);
            },
            Err(_) => {
                assert!(result.is_err());
                assert!(match result.unwrap_err() {
                    super::FromRfc1751Error::InvalidWord(w) => (word == w),
                    _ => false
                });
            }
        }
    }
}
