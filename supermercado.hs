import System.IO

type Codigo = Int
type Nome = String
type Preco = Float

type Produto = (Codigo, Nome, Preco)

tabelaProdutos :: [Produto]
tabelaProdutos =
  [ (001, "Chocolate", 5.25)
  , (002, "Biscoito", 10.10)
  , (003, "Laranja", 4.60)
  , (004, "Sabao", 2.10)
  , (005, "Batata Chips", 6.90)
  , (006, "Doritos", 8.90)
  ]

isCodigo :: Codigo -> Produto -> Bool
isCodigo codigo (cod, _, _) = codigo == cod

getPreco :: Produto -> Preco
getPreco (_, _, preco) = preco

getNome :: Produto -> Nome
getNome (_, nome, _) = nome

buscaPrecoPorCodigo :: Codigo -> Preco
buscaPrecoPorCodigo codigo = buscaPreco tabelaProdutos
  where
    buscaPreco ((cod, _, preco) : produtos)
      | codigo == cod = preco
      | otherwise = buscaPreco produtos

buscaNomePorCodigo :: Codigo -> Nome
buscaNomePorCodigo codigo = buscaNome tabelaProdutos
  where
    buscaNome ((cod, nome, _) : produtos)
      | codigo == cod = nome
      | otherwise = buscaNome produtos

calculaPrecos :: [Codigo] -> Preco
calculaPrecos codigos = sum $ map buscaPrecoPorCodigo codigos
  where
    buscaPrecoPorCodigo codigo = buscaPreco tabelaProdutos
      where
        buscaPreco ((cod, _, preco) : produtos)
          | codigo == cod = preco
          | otherwise = buscaPreco produtos

formataStrProduto :: Codigo -> String
formataStrProduto codigo = nome ++ pontos ++ precoStr ++ "\n"
  where
    nome = buscaNomePorCodigo codigo
    preco = buscaPrecoPorCodigo codigo
    precoStr = show preco
    pontos = replicate (30 - length nome - length precoStr) '.'

geraNotaFiscal :: [Codigo] -> IO ()
geraNotaFiscal codigos = do
  let linhasProdutos = map formataStrProduto codigos
  let total = calculaPrecos codigos
  let linhaTotal = "Total" ++ replicate (30 - length "Total") '.' ++ show total
  writeFile "nota_fiscal.txt" $ unlines (linhasProdutos ++ [linhaTotal])