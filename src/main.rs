use std::io;
use std::fmt;






struct Parser {
    tokens: Vec<Token>,
    idx: i32,
    current: Token
}


struct Interpreter {}



impl Interpreter {

    fn new() -> Result<Self, Error>{
        return Ok(Self {})
    }

    fn visit(&mut self, node: Node) {
        match node {
            Node::BinaryOperation(_) => return self.visit_binary_operation(node),
            Node::UnaryOperation(_) => return self.visit_unary_operation(node),
            Node::Number(_) => return self.visit_number(node),
            Node::Operator(_) => return self.visit_operator(node),
        }
    }

    fn visit_number(&mut self, node: Node) {
        println!("found number")
    }
    fn visit_operator(&mut self, node: Node) {
        println!("found number")
    }
    fn visit_binary_operation(&mut self, node: Node) {
        println!("found number");
        self.visit(get!(node, "left"))
    }
    fn visit_unary_operation(&mut self, node: Node) {
        println!("found number");
        self.visit(get!(node, "left"))
    }
}











impl Parser {
    fn new(tokens: Vec<Token>) -> Result<Self, Error> {
        let mut parser = Self { tokens, idx: -1, current: Token::Null };
        parser.advance()?;
        println!("all tokens: {:?}", parser.tokens);
        Ok(parser)
        
    }

    fn parse(&mut self) -> Result<Node, Error> {
        let result = self.expr();
        result
    }



    fn advance(&mut self) -> Result<Option<Token>, Error> {
        self.idx += 1;
        if self.idx < self.tokens.len() as i32 {
            self.current = self.tokens[self.idx as usize];
            Ok(Some(self.tokens[self.idx as usize]))
        } else {
            Ok(None)
        }
    }



    fn factor(&mut self) -> Result<Node, Error> {
        let token = self.current.clone();

        match token { 
            Token::Operator(Operator::Add) | Token::Operator(Operator::Sub) => {
                self.advance()?;
                let operator = mknode!(token).expect("mknode returned none");
                let other = self.factor()?;
                return Ok(Node::UnaryOperation(Box::new(UnaryOperation { operator, other})))
            },
            Token::Number(x) => {
                self.advance()?;
                return Ok(Node::Number(x))
            },

            Token::Bracket('(') => {
                self.advance()?;
                let expr = self.expr()?;

                if let Token::Bracket(')') = self.current {
                    self.advance()?;
                    return Ok(expr)
                } else {
                    return Err(Error::InvalidSyntax(0))
                }
            },
            _ => return Err(Error::InvalidSyntax(0))
        }
    }



    fn term(&mut self) -> Result<Node, Error> {

        let mut left = self.factor()?;

        while let Token::Operator(Operator::Div) 
                | Token::Operator(Operator::Mul) 
                | Token::Operator(Operator::Mod) 
                | Token::Operator(Operator::Qot) = self.current {

            let current = self.current;
            self.advance()?;
            let operator = mknode!(current).expect("mknode function returned None");
            let right = self.factor()?;

            left = Node::BinaryOperation(Box::new(BinaryOperation { left: left.clone(), operator, right }));
        }
        Ok(left)
    }



    fn expr(&mut self) -> Result<Node, Error> {
        let mut left = self.term()?;

        while let Token::Operator(Operator::Sub) | Token::Operator(Operator::Add) = self.current {
            let current = self.current;
            self.advance()?;
            let operator = mknode!(current).expect("mknode returned None");
            let right = self.term()?;

            left = Node::BinaryOperation(Box::new(BinaryOperation { left: left.clone(), operator, right }));
        }
        Ok(left)
    }
}



#[macro_export]
macro_rules! mknode {
    ($token:expr) => {
        match $token {
            Token::Operator(x) => Some(Node::Operator(x)),
            Token::Number(x) => Some(Node::Number(x)),
            _ => None
        }
    };
}

#[macro_export]
macro_rules! get {
    ($node:expr, $other:expr) => {
        match $other {
            "left" => {
                if let Node::BinaryOperation(x) = $node {
                    Some(x.left)
                } else {
                    NOne
                }
            },
            "right" => {
                if let Node::BinaryOperation(x) = $node {
                    Some(x.right)
                } else {
                    None
                }
            },
            "other" => {
                if let Node::UnaryOperation(x) = $node {
                    Some(x.other)
                } else {
                    None
                }
            }
            _ => 
            
        }
    }
    ($node:expr) => {

    }
}






fn main() {
    let mut equation = String::new();
    io::stdin().read_line(& mut equation).expect("failed to input equation");
    
    let tokens = match tokenise(&equation) {
        Ok(token_ls) => token_ls,
        Err(error) => panic!("failed to tokenise equation -> {:?}", error),
    };

    let mut parser = Parser::new(tokens.clone()).expect("failed to initialise parser");
    let ast = parser.parse().expect("parsing failed!");

    let mut interpreter = Interpreter::new().expect("failed to initialise interpreter");
    interpreter.visit(ast.clone());

    println!("AST : {:#?}", ast);

}



fn tokenise(equation: &str) -> Result<Vec<Token>, Error> {
    let mut tokens = Vec::new();
    let mut current_num = "".to_string();

    'mainloop: for (x, character) in equation.chars().enumerate() {


        match character {
            '0'..='9' => current_num.push(character),
            '.' => current_num.push(character),
            _ => {
                if current_num.len() != 0 {
                    tokens.push(Token::Number(current_num.parse::<f64>().unwrap()));
                    current_num = "".to_string();
                } match character {
                    '+' => tokens.push(Token::Operator(Operator::Add)),
                    '-' => tokens.push(Token::Operator(Operator::Sub)),
                    '%' => tokens.push(Token::Operator(Operator::Mod)),
                    '*' => {
                        if &equation.chars().nth(x-1).unwrap() == &'*' {
                            tokens.push(Token::Operator(Operator::Exp));
                        } else if &equation.chars().nth(x+1).unwrap() == &'*' {
                            ()
                        } else {
                            tokens.push(Token::Operator(Operator::Mul));
                        }
                    },
                    '/' => {
                        if &equation.chars().nth(x-1).unwrap() == &'/' {
                            tokens.push(Token::Operator(Operator::Qot));
                        } else if &equation.chars().nth(x+1).unwrap() == &'/' {
                            ()
                        } else {
                            tokens.push(Token::Operator(Operator::Div));
                        }
                    },
                    '(' | ')' | '[' | ']' | '{' | '}' | '<' | '>' => tokens.push(Token::Bracket(character)),
                    '\n' => break 'mainloop,
                    ' ' => (),
                    _ => {
                        return Err(Error::InvalidCharacter(x))
                    },
                }
            }
        } 
    }
    Ok(tokens)
}











#[derive(Copy, Debug, Clone, PartialEq)]
enum Token {
    Number(f64),
    Operator(Operator),
    Bracket(char),
    Null,
}


#[derive(Copy, Debug, Clone, PartialEq)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Qot,
    Mod,
    Exp,
}

#[derive(Debug)]
enum Error {
    InvalidSyntax(usize),
    InvalidCharacter(usize)
}



#[derive(Debug, Clone, PartialEq)]
enum Node {
    Number(f64),
    Operator(Operator),
    BinaryOperation(Box<BinaryOperation>),
    UnaryOperation(Box<UnaryOperation>)
}


#[derive(Debug, Clone, PartialEq)]
struct BinaryOperation {
    left: Node,
    operator: Node,
    right: Node,
}

#[derive(Debug, Clone, PartialEq)]
struct UnaryOperation {
    operator: Node,
    other: Node,
}







impl fmt::Display for BinaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f, "(\n{} \n{} \n{}\n)

            ", self.left, self.operator, self.right
        )
    }
}
impl fmt::Display for UnaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f, "(\n{} \n{} \n)

            ", self.operator, self.other,
        )
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Number(x) => write!(f, "{}", x),
            Node::Operator(x) => write!(f, "{}", x),
            Node::BinaryOperation(x) => {
                let inner = *x.clone();
                write!(f, "{}", inner)
            }
            Node::UnaryOperation(x) => {
                let inner = *x.clone();
                write!(f, "{}", inner)
            }
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operator::Add => write!(f, "+"),
            Operator::Sub => write!(f, "-"),
            Operator::Mul => write!(f, "*"),
            Operator::Div => write!(f, "/"),
            Operator::Mod => write!(f, "%"),
            Operator::Qot => write!(f, "//"),
            Operator::Exp => write!(f, "**"),
        }
    }
}



