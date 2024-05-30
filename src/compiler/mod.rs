pub mod ir;
pub mod pool;

pub struct Compiler {
    user_prefix: String,

    function_definitions: String,
    function_declarations: String,
}

// helpers
#[rustfmt::skip]
impl Compiler {
    const VALUE_TYPE: &'static str = "Value";
    const RESULT_TYPE: &'static str = "Result";
    const TRY: &'static str = "Try";
    const OK: &'static str = "Ok";
    const EXPECT_BOOL: &'static str = "EXPECT_BOOL";

    fn unit() -> String { String::from("Unit()") }
    fn bool(value: bool) -> String { format!("Bool({})", value) }
    fn int(value: i64) -> String { format!("Int({})", value) }
    fn float(value: f64) -> String { format!("Float({})", value) }
    fn string(value: &str) -> String { format!("String({:?})", value) }
   
    const BUILTIN_FUNCTIONS: &'static [&'static str] = &[
        "print",
        "println",
    ];
}
