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

impl ToRfc1751 for [u8] {
    fn to_rfc1751(&self) -> Result<String, ToRfc1751Error> {
        if self.len() % 8 != 0 {
            return Err(self::ToRfc1751Error::NotMultipleOfEight);
        }

        let mut result = String::new();
        let mut build: usize = 0;
        let mut have = 0;
        for current in self.iter() {
            have += 8;
            println!("START build = {0:011b}", build);
            println!("current = {0:08b}", current);
            if have > 11 {
                let d = have - 11;
                println!("have = {} > 11, PREP", have);
                println!("d = {}", d);
                build += (current >> d) as usize;
                println!("FINAL build = {0:011b}", build);
                println!("              aka. {}: '{}'", build, WORDS[build]);
                result.push_str(WORDS[build]);
                result.push_str(" ");
                // reset and carry over if necessary
                build = (*current as usize % (2 << (d-1))) * (2 << ((11-d)-1));
                have = d
            } else {
                let d = 11 - have;
                println!("have = {} < 11", have);
                println!("d = {}", d);
                build += (*current as usize) * (2 << (d-1));
            }
        }
        result.push_str(WORDS[build]);
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        use super::ToRfc1751;
        let target = [0xEB, 0x33, 0xF7, 0x7E, 0xE7, 0x3D, 0x40, 0x53];;
        let result = target.to_rfc1751();
        assert!(result.is_ok());
        assert_eq!("TIDE ITCH SLOW REIN RULE MOT", result.unwrap());
    }
}
