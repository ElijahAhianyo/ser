use crate::function::BuiltinFunction;
use std::time::Instant;
use crate::ast::LiteralObject;
use std::time::{SystemTime, UNIX_EPOCH};
use std::convert::TryFrom;

fn now_unix_secs_u32() -> Result<u64, &'static str> {
    let dur = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|_| "system time is before Unix epoch")?;
    Ok(dur.as_secs())
    // u32::try_from(secs).map_err(|_| "overflow converting seconds to u32")
}


pub fn clock() -> BuiltinFunction {
    BuiltinFunction::new(
        "clock".to_string(),
        0,
        |interpreter, args| {
            let tn = now_unix_secs_u32().expect("could not get current time");
            Ok(LiteralObject::Number(tn as f64))
        }
    )
}