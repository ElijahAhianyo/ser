use crate::ast::{Expr, LiteralObject};
use crate::interpreter::Interpreter;
use crate::interpreter::RuntimeError;
use std::fmt::Display;
pub(crate) trait Callable: Display {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<LiteralObject>,
    ) -> Result<LiteralObject, RuntimeError>;
    fn arity(&self) -> usize;

    fn name(&self) -> Option<String> {
        None
    }
}
