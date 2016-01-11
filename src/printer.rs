use parser::*;

struct StringWriter {
    buf: String,
    indent_level: i32,
}

impl StringWriter {
    fn new(indent: i32) -> StringWriter {
        StringWriter {
            buf: String::new(),
            indent_level: indent,
        }
    }

    fn write_indent(&mut self) {
        if self.indent_level <= 0 {
            return;
        }

        for _ in 0..self.indent_level {
            // Hardcoded 4 space tabs
            self.write("    ");
        }
    }

    fn write(&mut self, s: &str) {
        self.buf.push_str(s);
    }

    fn write_line(&mut self, s: &str) {
        self.do_line(&{}, |_, writer| {
            writer.write(s);
        });
    }

    fn do_line<T, V>(&mut self, context: &V, block: T)
        where T: Fn(&V, &mut StringWriter) -> ()
    {
        self.write_line_n(context, block, false);
    }

    fn write_line_n<T, V>(&mut self, context: &V, block: T, skip_newline: bool)
        where T: Fn(&V, &mut StringWriter) -> ()
    {
        if !skip_newline {
            self.buf.push('\n');
        }
        self.write_indent();
        block(context, self);
    }

    fn do_indent<T, V>(&mut self, context: &V, block: T)
        where T: Fn(&V, &mut StringWriter) -> ()
    {
        self.indent_level = self.indent_level + 1;
        block(context, self);
        self.indent_level = self.indent_level - 1;
    }
}

trait Print {
    fn print(&self, writer: &mut StringWriter) -> ();
}

impl<'a> Print for Block<'a> {
    fn print(&self, writer: &mut StringWriter) {
        writer.do_indent(&self, |context, writer| {
            for statement in &context.statements {
                statement.print(writer);
            }
            match context.return_statement {
                Some(ref statement) => {
                    writer.do_line(&{}, |_, writer| {
                        writer.write("return");
                        statement.print(writer);
                    });
                }
                None => (),
            }
        });
    }
}

impl<'a> Print for Statement<'a> {
    fn print(&self, writer: &mut StringWriter) {
        match *self {
            Statement::Semicolon => writer.write(";"),
            Statement::VariableAssignmentList { ref variables, ref expressions } => {
                variables.print(writer);
                writer.write(" = ");
                expressions.print(writer);
            }
            Statement::FunctionCall(ref call) => call.print(writer),
            Statement::Label(ref l) => l.print(writer),
            Statement::Break => writer.write("break"),
            Statement::Goto(ref name) => {
                writer.write("goto ");
                name.print(writer);
            }
            Statement::Do(ref block) => {
                writer.write_line("do");
                block.print(writer);
                writer.write_line("end");
            }
            Statement::While{ref expression, ref block} => {
                writer.do_line(&{}, |_, writer| {
                    writer.write("while ");
                    expression.print(writer);
                    writer.write(" do");
                });
                block.print(writer);
                writer.write_line("end");
            }
            Statement::Conditional{ref expression, ref block, ref elseifs, ref otherwise} => {
                writer.do_line(&{}, |_, writer| {
                    writer.write("if ");
                    expression.print(writer);
                    writer.write(" then");
                });
                block.print(writer);
                for elseif in elseifs {
                    writer.do_line(&{}, |_, writer| {
                        writer.write("elseif ");
                        elseif.expression.print(writer);
                        writer.write(" then");
                    });
                    elseif.body.print(writer);
                }
                match *otherwise {
                    Some(ref block) => {
                        writer.write_line("else");
                        block.print(writer);
                    }
                    None => (),
                }
                writer.write_line("end");
            }
            Statement::ForEquals{ref name, ref initializer, ref limit, ref step, ref block} => {
                writer.do_line(&{}, |_, writer| {
                    writer.write("for ");
                    name.print(writer);
                    writer.write(" = ");
                    initializer.print(writer);
                    writer.write(", ");
                    limit.print(writer);
                    match *step {
                        Some(ref step) => {
                            writer.write(", ");
                            step.print(writer);
                        }
                        None => (),
                    }
                    writer.write(" do");
                });
                block.print(writer);
                writer.write_line("end");
            }
            Statement::ForIn{ref names, ref expressions, ref block} => {
                writer.do_line(&{}, |_, writer| {
                    writer.write("for ");
                    names.print(writer);
                    writer.write(" in ");
                    expressions.print(writer);
                    writer.write(" do");
                });
                block.print(writer);
                writer.write_line("end");
            }
            Statement::FunctionDeclaration{ref name, ref body} => {
                writer.do_line(&{}, |_, writer| {
                    writer.write("function ");
                    name.print(writer);
                });
                body.print(writer);
            }
            Statement::LocalFunctionDeclaration{ref name, ref body} => {
                writer.do_line(&{}, |_, writer| {
                    writer.write("local function ");
                    name.print(writer);
                });
                body.print(writer);
            }
            Statement::LocalVariableAssignment{ref names, ref initializers} => {
                writer.do_line(&{}, |_, writer| {
                    writer.write("local ");
                    names.print(writer);
                    match *initializers {
                        Some(ref initializers) => {
                            writer.write(" = ");
                            initializers.print(writer);
                        }
                        None => (),
                    }
                });
            }
        }
    }
}

impl<'a> Print for Name<'a> {
    fn print(&self, writer: &mut StringWriter) {
        writer.write(self.identifier);
    }
}

impl<'a> Print for LiteralString<'a> {
    // TODO: Actual sanitization/parsing/normalization
    fn print(&self, writer: &mut StringWriter) {
        writer.write("\"");
        writer.write(self.content);
        writer.write("\"");
    }
}

impl Print for Numeral {
    fn print(&self, writer: &mut StringWriter) {
        writer.write(&self.content.to_string());
    }
}

impl<'a> Print for Label<'a> {
    fn print(&self, writer: &mut StringWriter) {
        writer.write("::");
        self.name.print(writer);
        writer.write("::");
    }
}

impl<'a> Print for FunctionCall<'a> {
    fn print(&self, writer: &mut StringWriter) {
        match *self {
            FunctionCall::Unqualified(ref prefix, ref args) => {
                prefix.print(writer);
                args.print(writer);
            }
            FunctionCall::Qualified(ref prefix, ref name, ref args) => {
                prefix.print(writer);
                writer.write(":");
                name.print(writer);
                args.print(writer);
            }
        }
    }
}

impl<'a> Print for VarList<'a> {
    fn print(&self, writer: &mut StringWriter) {
        self.first.print(writer);
        for var in &self.remainder {
            writer.write(", ");
            var.print(writer);
        }
    }
}

impl<'a> Print for ExpressionList<'a> {
    fn print(&self, writer: &mut StringWriter) {
        self.first.print(writer);
        for var in &self.remainder {
            writer.write(", ");
            var.print(writer);
        }
    }
}

impl<'a> Print for NameList<'a> {
    fn print(&self, writer: &mut StringWriter) {
        self.first.print(writer);
        for var in &self.remainder {
            writer.write(", ");
            var.print(writer);
        }
    }
}

impl<'a> Print for ReturnStatement<'a> {
    fn print(&self, writer: &mut StringWriter) {
        match self.expressions {
            Some(ref list) => {
                writer.write(" ");
                list.print(writer);
            }
            None => (),
        }
    }
}

impl<'a> Print for Expression<'a> {
    fn print(&self, writer: &mut StringWriter) {
        match *self {
            Expression::Nil => writer.write("nil"),
            Expression::False => writer.write("false"),
            Expression::True => writer.write("true"),
            Expression::Numeral(ref num) => num.print(writer),
            Expression::LiteralString(ref string) => string.print(writer),
            Expression::Ellipsis => writer.write("..."),
            Expression::FunctionDefinition(ref def) => def.print(writer),
            Expression::PrefixExpression(ref prefix) => prefix.print(writer),
            Expression::TableConstructor(ref ctor) => ctor.print(writer),
            Expression::BinaryOperator(ref left, ref op, ref right) => {
                left.print(writer);
                writer.write(" ");
                op.print(writer);
                writer.write(" ");
                right.print(writer);
            }
            Expression::UnaryOperator(ref op, ref expression) => {
                op.print(writer);
                expression.print(writer);
            }
        }
    }
}

impl<'a> Print for FunctionBody<'a> {
    fn print(&self, writer: &mut StringWriter) {
        writer.write("(");
        match self.parameters {
            Some(ref params) => {
                params.print(writer);
            }
            None => (),
        }
        writer.write(")");
        self.body.print(writer);
        writer.write_line("end");
    }
}

impl<'a> Print for FunctionName<'a> {
    fn print(&self, writer: &mut StringWriter) {
        self.root.print(writer);
        for qualifier in &self.qualifiers {
            writer.write(".");
            qualifier.print(writer);
        }
        match self.self_qualifier {
            Some(ref qualifier) => {
                writer.write(":");
                qualifier.print(writer);
            }
            None => (),
        }
    }
}

impl<'a> Print for FunctionArguments<'a> {
    fn print(&self, writer: &mut StringWriter) {
        match *self {
            FunctionArguments::ArgumentList(ref explist) => {
                writer.write("(");
                match *explist {
                    Some(ref params) => {
                        params.print(writer);
                    }
                    None => (),
                }
                writer.write(")");
            }
            FunctionArguments::TableConstructor(ref ctor) => ctor.print(writer),
            FunctionArguments::LiteralString(ref string) => string.print(writer),
        }
    }
}

impl<'a> Print for PrefixExpression<'a> {
    fn print(&self, writer: &mut StringWriter) {
        match *self {
            PrefixExpression::Var(ref var) => var.print(writer),
            PrefixExpression::FunctionCall(ref call) => call.print(writer),
            PrefixExpression::ParenthesisedExpression(ref expr) => {
                writer.write("(");
                expr.print(writer);
                writer.write(")");
            }
        }
    }
}

impl<'a> Print for Var<'a> {
    fn print(&self, writer: &mut StringWriter) {
        match *self {
            Var::Named(ref name) => name.print(writer),
            Var::ExpressionQualified(ref prefix, ref expr) => {
                prefix.print(writer);
                writer.write("[");
                expr.print(writer);
                writer.write("]");
            }
            Var::NameQualified(ref prefix, ref name) => {
                prefix.print(writer);
                writer.write(".");
                name.print(writer);
            }
        }
    }
}

impl Print for UnaryOperator {
    fn print(&self, writer: &mut StringWriter) {
        match *self {
            UnaryOperator::Minus => writer.write("-"),
            UnaryOperator::Not => writer.write("not "),
            UnaryOperator::Length => writer.write("#"),
            UnaryOperator::BinaryNegate => writer.write("~"),
        }
    }
}

impl Print for BinaryOperator {
    fn print(&self, writer: &mut StringWriter) {
        match *self {
            BinaryOperator::Add => writer.write("+"),
            BinaryOperator::Subtract => writer.write("-"),
            BinaryOperator::Multiply => writer.write("*"),
            BinaryOperator::Divide => writer.write("/"),
            BinaryOperator::DivideRounded => writer.write("//"),
            BinaryOperator::Exponentiate => writer.write("^"),
            BinaryOperator::Modulus => writer.write("%"),
            BinaryOperator::BinaryAnd => writer.write("&"),
            BinaryOperator::BinaryOr => writer.write("|"),
            BinaryOperator::RightShift => writer.write(">>"),
            BinaryOperator::LeftShift => writer.write("<<"),
            BinaryOperator::Concatenate => writer.write(".."),
            BinaryOperator::LessThan => writer.write("<"),
            BinaryOperator::LessThanOrEqualTo => writer.write("<="),
            BinaryOperator::GreaterThan => writer.write(">"),
            BinaryOperator::GreaterThanOrEqualTo => writer.write(">="),
            BinaryOperator::Equal => writer.write("=="),
            BinaryOperator::NotEqual => writer.write("~="),
            BinaryOperator::And => writer.write("and"),
            BinaryOperator::Or => writer.write("or"),
        }
    }
}

impl<'a> Print for FunctionDefinition<'a> {
    fn print(&self, writer: &mut StringWriter) {
        writer.write("function");
        self.func.print(writer);
    }
}

impl<'a> Print for Field<'a> {
    fn print(&self, writer: &mut StringWriter) {
        match *self {
            Field::CalculatedNameInitializer(ref calc, ref init) => {
                writer.write("[");
                calc.print(writer);
                writer.write("] = ");
                init.print(writer);
            }
            Field::Initializer(ref name, ref init) => {
                name.print(writer);
                writer.write(" = ");
                init.print(writer);
            }
            Field::IndexedInitializer(ref exp) => exp.print(writer),
        }
    }
}

impl<'a> Print for FieldList<'a> {
    fn print(&self, writer: &mut StringWriter) {
        writer.do_indent(self, |context, writer| {
            writer.do_line(self, |context, writer| {
                context.first.print(writer);
            });
            for field in &context.remainder {
                // Hardcoded comma field seperator
                writer.write(",");
                writer.do_line(&field, |field, writer| {
                    field.print(writer);
                });
            }
        });
    }
}

impl<'a> Print for TableConstructor<'a> {
    fn print(&self, writer: &mut StringWriter) {
        match self.field_list {
            Some(ref list) => {
                writer.write("{");
                list.print(writer);
                writer.write_line("}");
            }
            None => writer.write("{}"),
        }
    }
}

impl<'a> Print for ParameterList<'a> {
    fn print(&self, writer: &mut StringWriter) {
        match *self {
            ParameterList::Arguments(ref names, ref option) => {
                names.print(writer);
                match *option {
                    Some(_) => writer.write(", ..."),
                    None => (),
                }
            }
            ParameterList::Ellipsis => writer.write("..."),
        }
    }
}

pub fn pretty_print(ast: Block) -> String {
    // Start indent level at -1 so the block representing the top chunk isn't indented
    let mut writer = StringWriter::new(-1);
    ast.print(&mut writer);
    return writer.buf;
}


#[test]
fn test_print() {
    let target = r#"
local x = 2;
local y = 4;
function add()
    return x + y
end"#;
    let mut writer = StringWriter::new(-1);
    let ast = Block {
        statements: vec![
            Statement::LocalVariableAssignment{
                names: NameList {
                    first: Name {
                        identifier: "x"
                    },
                    remainder: vec![]
                },
                initializers: Some(ExpressionList {
                    first: Expression::Numeral(Numeral {
                        content: 2.0
                    }),
                    remainder: vec![]
                })
            },
            Statement::Semicolon,
            Statement::LocalVariableAssignment{
                names: NameList {
                    first: Name {
                        identifier: "y"
                    },
                    remainder: vec![]
                },
                initializers: Some(ExpressionList {
                    first: Expression::Numeral(Numeral {
                        content: 4.0
                    }),
                    remainder: vec![]
                })
            },
            Statement::Semicolon,
            Statement::FunctionDeclaration{
                name: FunctionName {
                    root: Name {
                        identifier: "add"
                    },
                    qualifiers: vec![],
                    self_qualifier: None,
                },
                body: FunctionBody {
                    parameters: None,
                    body: Block {
                        statements: vec![],
                        return_statement: Some(ReturnStatement {
                            expressions: Some(ExpressionList {
                                first: Expression::BinaryOperator(
                                    Box::new(Expression::PrefixExpression(Box::new(PrefixExpression::Var(Box::new(Var::Named(Name {
                                        identifier: "x",
                                    })))))),
                                    BinaryOperator::Add,
                                    Box::new(Expression::PrefixExpression(Box::new(PrefixExpression::Var(Box::new(Var::Named(Name {
                                        identifier: "y",
                                    }))))))
                                ),
                                remainder: vec![],
                            })
                        })
                    }
                }
            }
        ],
        return_statement: None,
    };
    ast.print(&mut writer);
    assert_eq!(writer.buf, target)
}
