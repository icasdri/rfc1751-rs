mod words;
use words::WORDS;

pub enum FromRfc1751Error {

}

pub enum ToRfc1751Error {
    NotMultipleOfEight
}

pub trait FromRfc1751 {
    fn from_rfc1751(&self) -> Result<Vec<u8>, FromRfc1751Error>;
}

pub trait ToRfc1751 {
    fn to_rfc1751(&self) -> Result<String, ToRfc1751Error>;
}

impl ToRfc1751 for [u8] {
    fn to_rfc1751(&self) -> Result<String, ToRfc1751Error> {
        if self.len() % 8 != 0 {
            return Err(self::ToRfc1751Error::NotMultipleOfEight);
        }

        let mut result = String::new();
        let mut build= 0;
        let mut have = 0;
        for current in self.iter() {
            have += 8;
            if have > 11 {
                let d = have - 11;
                build += (current >> d) as usize;
                result.push_str(WORDS[build]);
                result.push_str(" ");
                // reset and carry over if necessary
                build = (current << d) as usize;
                have -= 11
            } else {
                let d = 11 - have;
                build += (current << d) as usize;
            }
        }
        // pop the trailing space
        result.pop();
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        use super::ToRfc1751;
        let target = [0xEB, 0x33, 0xF7, 0x7E, 0xE7, 0x3D, 0x40, 0x53];;
        assert_eq!("TIDE ITCH SLOW REIN RULE MOT", target.to_rfc1751());
    }
}
