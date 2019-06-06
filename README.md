# ImpCompiler

Projeto Construindo um compilador para a linguagem Imp , utilizando o Pi Framework na linguagem Idris.

# Idris
Pode-se encontrar mais sobre a linguagem no link https://www.idris-lang.org/

## Instalação do Idris
make install

##Execução
Para execução do piAutomata deve rodar os seguintes comandos:
cd src
idris -p contrib piAutomata.idr

E chamar a função process com os programas desejados, escritos em piIR. Um exemplo é o seguinte programa:

x = 5
y = 3
while x>2
  y = y+10
  x = x-1

Para testá-lo deve chamar a função process da seguinte forma:

process ( [CtCmd (CSeq (Assign (ValID "x") (AExpR (N 5))) (CSeq (Assign (ValID"y") (AExpR (N 3))) (Loop (GT (ID (ValID "x")) (N 2)) (CSeq (Assign (ValID "y") (AExpR (Sum (ID (ValID "y")) (N 10)))) (Assign (ValID "x") (AExpR (Sub (ID (ValID "x")) (N 1))))))))],[],fromList [ (ValId "x", L 1) , (ValId "y", L 2) , (ValId "z", L 3)], empty, [])

Para encerrar a execução :q

###Parser
Por enquanto, o parser não está concluído, só temos o lexer pronto.
Para executa-lo deve rodar os seguintes comandos:

cd teste
idris main.idr
:exec

Ao fazer isso será pedido para inserir o programa, um exemplo é: x:= (2+3)* 5

Para encerrar a leitura Ctrl+C
Para encerrar a execução :q
