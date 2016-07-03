mod words;
use words::WORDS;

#[derive(Debug)]
pub enum FromRfc1751Error {

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

    use super::ToRfc1751;

    fn to_subkey_test(target: &[u8], expected: &'static str) {
        let mut result = String::new();
        super::to_rfc1751_transform_append_subkey(target, &mut result);
        assert_eq!(result, expected);
    }

}
