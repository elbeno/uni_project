/* parser.c */

#include "global.h"

int lookahead;

void parse()
{
  lookahead = lexan();
  while (lookahead != DONE)
    {
    expr();
    match(';');
    }
}

expr()
{
  int t;
  term();
  while(1)
    switch (lookahead)
      {
      case '+':
      case '-': t = lookahead;
                match(lookahead);
                term();
                emit(t,NONE);
                continue;
      default:  return;
      }
}

term()
{
  int t;
  factor();
  while(1)
    switch (lookahead)
      {
      case '*':
      case '/':
      case DIV:
      case MOD: t = lookahead;
                match(lookahead);
                factor();
                emit(t,NONE);
                continue;
      default:  return;
      }
}

factor()
{
  int t;
  index();
  while(1)
    switch (lookahead)
      {
      case '^': t = lookahead;
                match(lookahead);
                factor();
                emit(t,NONE);
                continue;
      default:  return;
      }
}

index()
{
  int t;
  switch (lookahead)
    {
    case '-': t = lookahead;
              match(lookahead);
              atom();
              emit('~',NONE);
              return;
    default:  atom();
              return;
    }
}

atom()
{
  int t;
  switch (lookahead)
    {
    case SIN:
    case COS: t = lookahead;
              match(lookahead);
              match('(');
              expr();
              match(')');
              emit(t,NONE);
              break;
    case '(': match('(');
              expr();
              match(')');
              break;
    case NUM: emit(NUM, tokenval);
              match(NUM);
              break;
    case ID:  emit(ID, tokenval);
              match(ID);
              break;
    default:  error("syntax error");
    }
}

match(int t)
{
  if (lookahead==t)
    lookahead = lexan();
  else
    error("syntax error");
}
