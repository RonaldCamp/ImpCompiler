# ImpCompiler

Projeto Construindo um compilador para a linguagem Imp , utilizando o Pi Framework na linguagem Idris.

# Idris
Pode-se encontrar mais sobre a linguagem no link https://www.idris-lang.org/

## Instalação do Idris
```
make all
```

## Execução
Para execução da aplicação deve rodar os seguintes comandos:
```
cd src
idris -p contrib main.idr
:exec
```

Ao fazer isso será pedido para inserir o programa, um exemplo é: let var z:=1 in let rec fn f(x) = let var y:=x in if y>=1 then z:=z*y, f(y-1) in f(10)

No momento, não é possível lidar com quebra de linha

```
let var z:=1 in
  let rec fn f(x) =
    let var y:=x in
      if y>=1 then
        z:=z*y
        f(y-1)
  in f(2)
```

Como padrão, o traço de execução é mostrado apenas com os dois últimos estados do automato

Para exibir todo o traço de execução, o comando "--all" pode ser inserido após o código imp a ser executado. Uma outra opção é o comando "--a" que exibe a arvore sintática gerada pelo parser.

Para encerrar a leitura Ctrl+C

Para encerrar a execução :q
