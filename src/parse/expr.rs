use super::error::{ParseError, ParseErrorKind};
use super::Context;
use crate::ast::{self};
use crate::lex::{Token, TokenKind};

impl<'a> Context<'a> {
    pub fn parse_expr(&mut self) -> Result<ast::Expression, ParseError> {
        match self.peek().kind {
            TokenKind::Number(n) => {
                let Token { span, .. } = self.next()?;
                Ok(ast::Expression {
                    kind: ast::ExpressionKind::Literal(n),
                    span,
                })
            }
            TokenKind::Symbol('{') => todo!(),
            TokenKind::Symbol('-') => todo!(),
            TokenKind::Symbol('(') => {
                let Token { span, .. } = self.next()?;
                let expr = self.parse_expr()?;
                self.expect(
                    &TokenKind::Symbol(')'),
                    &ParseErrorKind::MissingCloseParen { opening: span },
                )?;
                Ok(expr)
            }
            TokenKind::Symbol('^') => todo!(),
            TokenKind::Vec(_) => todo!(),
            TokenKind::Mat { .. } => todo!(),
            _ => todo!(),
        }
    }
}
