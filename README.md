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

Ao fazer isso será pedido para inserir o programa, um exemplo é: let var x:= 5 in let var y:=3 in while x>2 do y:=y+10, x:=x-1

No momento, não é possível lidar com quebra de linha

```
let var x := 5 in
  let var y := 3 in
    while x>2 do
      y := y+10
      x := x-1
```

Para encerrar a leitura Ctrl+C

Para encerrar a execução :q
