use crate::ast::LiteralObject;
use crate::callable::Callable;
use crate::function::UserDefinedFunction;
use crate::interpreter::{Interpreter, RuntimeError};
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Class {
    name: String,
    methods: HashMap<String, UserDefinedFunction>,
    super_class: Option<Box<Class>>,
}

impl Class {
    pub fn new(
        name: String,
        methods: HashMap<String, UserDefinedFunction>,
        super_class: Option<Class>,
    ) -> Self {
        Self {
            name,
            methods,
            super_class: super_class.map(Box::new),
        }
    }

    pub fn get_method<K: AsRef<str>>(&self, name: K) -> Option<&UserDefinedFunction> {
        let name = name.as_ref();
        if let Some(method) = self.methods.get(name) {
            return Some(method);
        }

        if let Some(super_class) = &self.super_class {
            return super_class.get_method(name);
        }

        None
    }

    pub fn name(&self) -> &String {
        &self.name
    }

    pub fn methods(&self) -> &HashMap<String, UserDefinedFunction> {
        &self.methods
    }

    pub fn super_class(&self) -> &Option<Box<Class>> {
        &self.super_class
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Callable for Class {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<LiteralObject>,
    ) -> Result<LiteralObject, RuntimeError> {
        let instance = Rc::new(RefCell::new(Instance::new(self.clone())));
        // if there is an init method, we should call it with the instance as the first argument
        if let Some(init) = self.get_method("init") {
            let mut init = init.clone();

            if let Ok(LiteralObject::Callable(binded)) = init.bind(instance.clone()) {
                if let Ok(LiteralObject::Instance(returned_instance)) =
                    binded.call(interpreter, args)
                {
                    // if the init method returns an instance, we should return that instance instead of the one we created.
                    // Typical eg. is when the init method returns "this", just use that.
                    return Ok(LiteralObject::Instance(returned_instance));
                }
            }
        }
        Ok(LiteralObject::Instance(instance.clone()))
    }

    fn arity(&self) -> usize {
        // the arity here matches the init method if it exists, otherwise 0
        if let Some(init) = self.get_method("init") {
            init.arity()
        } else {
            0
        }
    }

    fn name(&self) -> Option<&String> {
        Some(&self.name)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub struct Instance {
    class: Class,
    fields: HashMap<String, Option<LiteralObject>>,
}

impl Instance {
    pub fn new(class: Class) -> Self {
        Self {
            class,
            fields: HashMap::new(),
        }
    }

    pub fn get<K: AsRef<str>>(&mut self, name: K) -> Option<LiteralObject> {
        let name = name.as_ref();
        let val = self.fields.get(name);

        if let Some(val) = val {
            return val.clone();
        }

        // if we dont find the field in the instance, we should check if the class has a method with the same name
        // let mut method = self.class.get_method(name);

        let method_opt = { self.class.get_method(name).cloned() };

        if let Some(mut method) = method_opt {
            let instance = Rc::new(RefCell::new(self.clone()));
            return method.bind(instance).ok();
        }
        None
    }

    pub fn set<K: Into<String>>(&mut self, name: K, value: LiteralObject) {
        self.fields.insert(name.into(), Some(value));
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance", self.class)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub enum ClassKind {
    NONE,
    CLASS,
    SUBCLASS,
}
