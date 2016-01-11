// Lua's entire syntax:
//
// chunk ::= block
//
// block ::= {stat} [retstat]
//
// stat ::=  ‘;’ |
// varlist ‘=’ explist |
// functioncall |
// label |
// break |
// goto Name |
// do block end |
// while exp do block end |
// repeat block until exp |
// if exp then block {elseif exp then block} [else block] end |
// for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end |
// for namelist in explist do block end |
// function funcname funcbody |
// local function Name funcbody |
// local namelist [‘=’ explist]
//
// retstat ::= return [explist] [‘;’]
//
// label ::= ‘::’ Name ‘::’
//
// funcname ::= Name {‘.’ Name} [‘:’ Name]
//
// varlist ::= var {‘,’ var}
//
// var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name
//
// namelist ::= Name {‘,’ Name}
//
// explist ::= exp {‘,’ exp}
//
// exp ::=  nil | false | true | Numeral | LiteralString | ‘...’ | functiondef |
// prefixexp | tableconstructor | exp binop exp | unop exp
//
// prefixexp ::= var | functioncall | ‘(’ exp ‘)’
//
// functioncall ::=  prefixexp args | prefixexp ‘:’ Name args
//
// args ::=  ‘(’ [explist] ‘)’ | tableconstructor | LiteralString
//
// functiondef ::= function funcbody
//
// funcbody ::= ‘(’ [parlist] ‘)’ block end
//
// parlist ::= namelist [‘,’ ‘...’] | ‘...’
//
// tableconstructor ::= ‘{’ [fieldlist] ‘}’
//
// fieldlist ::= field {fieldsep field} [fieldsep]
//
// field ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp | exp
//
// fieldsep ::= ‘,’ | ‘;’
//
// binop ::=  ‘+’ | ‘-’ | ‘*’ | ‘/’ | ‘//’ | ‘^’ | ‘%’ |
// ‘&’ | ‘~’ | ‘|’ | ‘>>’ | ‘<<’ | ‘..’ |
// ‘<’ | ‘<=’ | ‘>’ | ‘>=’ | ‘==’ | ‘~=’ |
// and | or
//
// unop ::= ‘-’ | not | ‘#’ | ‘~’
//
// `Name`, `Numeral`, and `LiteralString` terminal tokens are defined outside this grammar
//

use nom::{alphanumeric, digit, is_alphabetic, is_digit, IResult, Err, ErrorKind};
use std::str;

// TODO: comments

// TODO: Other/real string syntaxes
pub struct LiteralString<'a> {
    pub content: &'a str,
}
named!(literal_string<&[u8], LiteralString>, alt!(
    chain!( tag!("'") ~ content: map_res!( alphanumeric, str::from_utf8 ) ~ tag!("'"), || { LiteralString {content: content} } ) |
    chain!( tag!("\"") ~ content: map_res!( alphanumeric, str::from_utf8 ) ~ tag!("\""), || { LiteralString {content: content} } )
));

// TODO: Other/real numeric syntaxes
pub struct Numeral {
    pub content: f64,
}
named!(numeral<&[u8], Numeral>, chain!( number: map_res!( digit, str::from_utf8 ), || { Numeral { content: number.parse::<f64>().unwrap() } } ) );

// TODO: Forbid reserved words
pub struct Name<'a> {
    pub identifier: &'a str,
}
// 0x5f is hex for _
pub fn name(input: &[u8]) -> IResult<&[u8], Name> {
    for (idx, item) in input.iter().enumerate() {
        if idx == 0 {
            if !is_alphabetic(*item) && *item != 0x5f {
                return IResult::Error(Err::Position(ErrorKind::Alpha, input));
            }
        } else {
            if !is_digit(*item) && !is_alphabetic(*item) && *item != 0x5f {
                return IResult::Done(&input[idx..],
                                     Name { identifier: str::from_utf8(&input[0..idx]).unwrap() });
            }
        }
    }
    IResult::Done(b"", Name { identifier: str::from_utf8(input).unwrap() })
}
// named!(name_start<&[u8], &[u8]>, alt!( alpha | tag!("_") ));
// named!(name_part<&[u8], &[u8]>, alt!( alphanumeric | tag!("_") ));
// named!(name<&[u8], Name>, chain!( start: name_start ~ parts: many0!( name_part ), || { Name { identifier: "" } } ));

pub struct Block<'a> {
    pub statements: Vec<Statement<'a>>,
    pub return_statement: Option<ReturnStatement<'a>>,
}
named!(block<&[u8], Block>, chain!( statements: many0!( stat ) ~ return_statement: retstat?, || { Block { statements: statements, return_statement: return_statement } } ));

pub enum UnaryOperator {
    Minus,
    Not,
    Length,
    BinaryNegate,
}
named!(unop<&[u8], UnaryOperator>, alt!( chain!( tag!("-"), || UnaryOperator::Minus ) | chain!( tag!("not"), || UnaryOperator::Not ) | chain!( tag!("#"), || UnaryOperator::Length ) | chain!( tag!("~"), || UnaryOperator::BinaryNegate ) ));

pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    DivideRounded,
    Exponentiate,
    Modulus,
    BinaryAnd,
    BinaryOr,
    RightShift,
    LeftShift,
    Concatenate,
    LessThan,
    LessThanOrEqualTo,
    GreaterThan,
    GreaterThanOrEqualTo,
    Equal,
    NotEqual,
    And,
    Or,
}
named!(binop<&[u8], BinaryOperator>, alt!(
    chain!( tag!("+"),  || BinaryOperator::Add ) |
    chain!( tag!("-"),  || BinaryOperator::Subtract ) |
    chain!( tag!("*"),  || BinaryOperator::Multiply ) |
    chain!( tag!("/"),  || BinaryOperator::Divide ) |
    chain!( tag!("//"), || BinaryOperator::DivideRounded ) |
    chain!( tag!("^"),  || BinaryOperator::Exponentiate ) |
    chain!( tag!("%"),  || BinaryOperator::Modulus ) |
    chain!( tag!("&"),  || BinaryOperator::BinaryAnd ) |
    /*tag!("~") |*/ // this has to be a bug in the lua spec - afaik there is no binay `~` op defined
    chain!( tag!("|"),  || BinaryOperator::BinaryOr ) |
    chain!( tag!(">>"), || BinaryOperator::RightShift ) |
    chain!( tag!("<<"), || BinaryOperator::LeftShift ) |
    chain!( tag!(".."), || BinaryOperator::Concatenate ) |
    chain!( tag!("<"),  || BinaryOperator::LessThan ) |
    chain!( tag!("<="), || BinaryOperator::LessThanOrEqualTo ) |
    chain!( tag!(">"),  || BinaryOperator::GreaterThan ) |
    chain!( tag!(">="), || BinaryOperator::GreaterThanOrEqualTo ) |
    chain!( tag!("=="), || BinaryOperator::Equal ) |
    chain!( tag!("~="), || BinaryOperator::NotEqual ) |
    chain!( tag!("and"),|| BinaryOperator::And ) |
    chain!( tag!("or"), || BinaryOperator::Or )
));

named!(fieldsep, alt!( tag!(",") | tag!(";") ));

pub enum Field<'a> {
    CalculatedNameInitializer(Expression<'a>, Expression<'a>),
    Initializer(Name<'a>, Expression<'a>),
    IndexedInitializer(Expression<'a>),
}
named!(field<&[u8], Field>, alt!(
    chain!( tag!("[") ~ access: exp ~ tag!("]") ~ tag!("=") ~ initializer: exp, || { Field::CalculatedNameInitializer(access, initializer) } ) |
    chain!( fieldname: name ~ tag!("=") ~ expression: exp, || { Field::Initializer(fieldname, expression) } ) |
    chain!( e: exp, || Field::IndexedInitializer(e) )
));

pub struct FieldList<'a> {
    pub first: Field<'a>,
    pub remainder: Vec<Field<'a>>,
}

named!(fieldlist<&[u8], FieldList>, chain!( first: field ~ rest: many0!( chain!( fieldsep ~ individual: field, || { individual } ) ) ~ fieldsep?, || { FieldList {first: first, remainder: rest} } ));

pub struct TableConstructor<'a> {
    pub field_list: Option<FieldList<'a>>,
}
named!(tableconstructor<&[u8], TableConstructor>, chain!( tag!("{") ~ fields: fieldlist? ~ tag!("}"), || { TableConstructor {field_list: fields} } ));

pub enum ParameterList<'a> {
    Arguments(NameList<'a>, Option<()>),
    Ellipsis,
}
named!(parlist<&[u8], ParameterList>, alt!( chain!( names: namelist ~ has_ellipsis: chain!( tag!(",") ~ tag!("..."), || {} )?, || { ParameterList::Arguments(names, has_ellipsis) } ) | chain!( tag!("..."), || ParameterList::Ellipsis ) ));

pub struct FunctionBody<'a> {
    pub parameters: Option<ParameterList<'a>>,
    pub body: Block<'a>,
}
named!(funcbody<&[u8], FunctionBody>, chain!( tag!("(") ~ parameters: parlist? ~ tag!(")") ~ body: block ~ tag!("end"), || { FunctionBody { parameters: parameters, body: body } } ));

pub struct FunctionDefinition<'a> {
    pub func: FunctionBody<'a>,
}
named!(functiondef<&[u8], FunctionDefinition>, chain!( tag!("function") ~ func: funcbody, || { FunctionDefinition {func: func} } ));

pub enum FunctionArguments<'a> {
    ArgumentList(Option<ExpressionList<'a>>),
    TableConstructor(TableConstructor<'a>),
    LiteralString(LiteralString<'a>),
}
named!(args<&[u8], FunctionArguments>, alt!(
    chain!( tag!("(") ~ list: explist? ~ tag!(")"), || { FunctionArguments::ArgumentList(list) } ) |
    chain!( t: tableconstructor, || FunctionArguments::TableConstructor(t) ) |
    chain!( l: literal_string, || FunctionArguments::LiteralString(l) )
));

pub enum FunctionCall<'a> {
    Unqualified(PrefixExpression<'a>, FunctionArguments<'a>),
    Qualified(PrefixExpression<'a>, Name<'a>, FunctionArguments<'a>),
}
named!(functioncall<&[u8], FunctionCall>, alt!(
    chain!( prefix: prefixexp ~ arg: args, || { FunctionCall::Unqualified(prefix, arg) } ) |
    chain!( prefix: prefixexp ~ tag!(":") ~ identifier: name ~ arg: args, || { FunctionCall::Qualified(prefix, identifier, arg) } )
));

pub enum PrefixExpression<'a> {
    Var(Box<Var<'a>>),
    FunctionCall(Box<FunctionCall<'a>>),
    ParenthesisedExpression(Expression<'a>),
}
named!(prefixexp<&[u8], PrefixExpression>, alt!(
    chain!( v: var, || PrefixExpression::Var(Box::new(v)) ) |
    chain!( f: functioncall, || PrefixExpression::FunctionCall(Box::new(f)) ) |
    chain!( tag!("(") ~ expression: exp ~ tag!(")"), || { PrefixExpression::ParenthesisedExpression(expression) } )
));

pub enum Expression<'a> {
    Nil,
    False,
    True,
    Numeral(Numeral),
    LiteralString(LiteralString<'a>),
    Ellipsis,
    FunctionDefinition(Box<FunctionDefinition<'a>>),
    PrefixExpression(Box<PrefixExpression<'a>>),
    TableConstructor(Box<TableConstructor<'a>>),
    BinaryOperator(Box<Expression<'a>>, BinaryOperator, Box<Expression<'a>>),
    UnaryOperator(UnaryOperator, Box<Expression<'a>>),
}
named!(exp<&[u8], Expression>, alt!(
    chain!( tag!("nil"), || Expression::Nil ) |
    chain!( tag!("false"), || Expression::False ) |
    chain!( tag!("true"), || Expression::True ) |
    chain!( n: numeral, || Expression::Numeral(n) ) |
    chain!( l: literal_string, || Expression::LiteralString(l) ) |
    chain!( tag!("..."), || Expression::Ellipsis ) |
    chain!( f: functiondef, || Expression::FunctionDefinition(Box::new(f)) ) |
    chain!( p: prefixexp, || Expression::PrefixExpression(Box::new(p)) ) |
    chain!( t: tableconstructor, || Expression::TableConstructor(Box::new(t)) ) |
    chain!( left: exp ~ op: binop ~ right: exp, || { Expression::BinaryOperator(Box::new(left), op, Box::new(right)) } ) |
    chain!( op: unop ~ expression: exp, || { Expression::UnaryOperator(op, Box::new(expression)) } )
));

pub struct ExpressionList<'a> {
    pub first: Expression<'a>,
    pub remainder: Vec<Expression<'a>>,
}
named!(explist<&[u8], ExpressionList>, chain!( first: exp ~ remainder: many0!( chain!( tag!(",") ~ expression: exp, || { expression } ) ), || { ExpressionList {first: first, remainder: remainder} } ));

pub struct NameList<'a> {
    pub first: Name<'a>,
    pub remainder: Vec<Name<'a>>,
}
named!(namelist<&[u8], NameList>, chain!( first: name ~ remainder: many0!( chain!( tag!(",") ~ identifier: name, || {identifier} ) ), || { NameList {first: first, remainder: remainder} } ));

pub enum Var<'a> {
    Named(Name<'a>),
    ExpressionQualified(PrefixExpression<'a>, Expression<'a>),
    NameQualified(PrefixExpression<'a>, Name<'a>),
}
named!(var<&[u8], Var>, alt!(
    chain!( n: name, || Var::Named(n) ) |
    chain!( prefix: prefixexp ~ tag!("[") ~ expression: exp ~ tag!("]"), || { Var::ExpressionQualified(prefix, expression) } ) |
    chain!( prefix: prefixexp ~ tag!(".") ~ identifier: name, || { Var::NameQualified(prefix, identifier) } )
));

pub struct VarList<'a> {
    pub first: Var<'a>,
    pub remainder: Vec<Var<'a>>,
}
named!(varlist<&[u8], VarList>, chain!( first: var ~ remainder: many0!( chain!( tag!(",") ~ declaration: var, || {declaration}) ), || { VarList {first: first, remainder: remainder} } ));

pub struct FunctionName<'a> {
    pub root: Name<'a>,
    pub qualifiers: Vec<Name<'a>>,
    pub self_qualifier: Option<Name<'a>>,
}
named!(funcname<&[u8], FunctionName>, chain!( root: name ~ qualifiers: many0!( chain!( tag!(".") ~ identifier: name, || { identifier } ) ) ~ self_qualifier: chain!( tag!(":") ~ identifier: name, || { identifier } )?, || { FunctionName {root: root, qualifiers: qualifiers, self_qualifier: self_qualifier} } ));

pub struct Label<'a> {
    pub name: Name<'a>,
}
named!(label<&[u8], Label>, chain!( tag!("::") ~ identifier: name ~ tag!("::"), || { Label {name: identifier} } ));

pub struct ReturnStatement<'a> {
    pub expressions: Option<ExpressionList<'a>>,
}
named!(retstat<&[u8], ReturnStatement>, chain!( tag!("return") ~ expressions: explist? ~ tag!(";")?, || { ReturnStatement {expressions: expressions} } ));

pub struct ConditionalBranch<'a> {
    pub expression: Expression<'a>,
    pub body: Block<'a>,
}

pub enum Statement<'a> {
    Semicolon,
    VariableAssignmentList {
        variables: VarList<'a>,
        expressions: ExpressionList<'a>,
    },
    FunctionCall(FunctionCall<'a>),
    Label(Label<'a>),
    Break,
    Goto(Name<'a>),
    Do(Block<'a>),
    While {
        expression: Expression<'a>,
        block: Block<'a>,
    },
    Conditional {
        expression: Expression<'a>,
        block: Block<'a>,
        elseifs: Vec<ConditionalBranch<'a>>,
        otherwise: Option<Block<'a>>,
    },
    ForEquals {
        name: Name<'a>,
        initializer: Expression<'a>,
        limit: Expression<'a>,
        step: Option<Expression<'a>>,
        block: Block<'a>,
    },
    ForIn {
        names: NameList<'a>,
        expressions: ExpressionList<'a>,
        block: Block<'a>,
    },
    FunctionDeclaration {
        name: FunctionName<'a>,
        body: FunctionBody<'a>,
    },
    LocalFunctionDeclaration {
        name: Name<'a>,
        body: FunctionBody<'a>,
    },
    LocalVariableAssignment {
        names: NameList<'a>,
        initializers: Option<ExpressionList<'a>>,
    },
}
named!(stat<&[u8], Statement>, alt!(
    chain!( tag!(";"), || Statement::Semicolon ) |
    chain!( variables: varlist ~ tag!("=") ~ expressions: explist, || { Statement::VariableAssignmentList{variables: variables, expressions: expressions} } ) |
    chain!( f: functioncall, || Statement::FunctionCall(f) ) |
    chain!( l: label, || Statement::Label(l) ) |
    chain!( tag!("break"), || Statement::Break ) |
    chain!( tag!("goto") ~ label: name, || { Statement::Goto(label) } ) |
    chain!( tag!("do") ~ content: block ~ tag!("end"), || { Statement::Do(content) } ) |
    chain!( tag!("while") ~ expression: exp ~ tag!("do") ~ content: block ~ tag!("end"), || { Statement::While{expression: expression, block: content} } ) |
    chain!( tag!("if") ~ expression: exp ~ tag!("then") ~ content: block ~ elseifs: many0!( chain!( tag!("elseif") ~ expression: exp ~ tag!("then") ~ body: block, || { ConditionalBranch {expression: expression, body: body} } ) ) ~ otherwise: chain!( tag!("else") ~ body: block, || { body } )? ~ tag!("end"), || { Statement::Conditional { expression: expression, block: content, elseifs: elseifs, otherwise: otherwise } } ) |
    chain!( tag!("for") ~ identifier: name ~ tag!("=") ~ initializer: exp ~ tag!(",") ~ limit: exp ~ step: chain!( tag!(",") ~ expression: exp, || { expression } )? ~ tag!("do") ~ body: block ~ tag!("end"), || { Statement::ForEquals{ name: identifier, initializer: initializer, limit: limit, step: step, block: body } } ) |
    chain!( tag!("for") ~ names: namelist ~ tag!("in") ~ expressions: explist ~ tag!("do") ~ body: block ~ tag!("end"), || { Statement::ForIn{names: names, expressions: expressions, block: body} } ) |
    chain!( tag!("function") ~ identifier: funcname ~ body: funcbody, || { Statement::FunctionDeclaration{name: identifier, body: body} } ) |
    chain!( tag!("local") ~ tag!("function") ~ identifier: name ~ body: funcbody, || { Statement::LocalFunctionDeclaration{name: identifier, body: body} } ) |
    chain!( tag!("local") ~ names: namelist ~ initializers: chain!( tag!("=") ~ list: explist, || { list } )?, || { Statement::LocalVariableAssignment{names: names, initializers: initializers} } )
));
named!(chunk<&[u8], Block>, alt!( block ));

pub fn parse<'a>(input: &'a str) -> Result<Block<'a>, &'static str> {
    match chunk(input.as_bytes()) {
        IResult::Done(_, r) => Result::Ok(r),
        IResult::Error(_) => Result::Err("Parse error"),
        IResult::Incomplete(_) => Result::Err("Parse incomplete"),
    }
}

#[cfg(test)]
mod tests {

}