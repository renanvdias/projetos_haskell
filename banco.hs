import System.IO
import System.Directory

imprime :: String -> IO ()
imprime path = do
  currentDir <- getCurrentDirectory
  let fullPath = currentDir ++ "/" ++ path
  conteudo <- readFile fullPath
  putStr conteudo

deposito :: Float -> IO ()
deposito valor = do
  currentDir <- getCurrentDirectory
  let saldoPath = currentDir ++ "/saldo.txt"
      extratoPath = currentDir ++ "/extrato.txt"
  saldoFile <- openFile saldoPath ReadMode
  saldoStr <- hGetLine saldoFile
  let saldo = read saldoStr :: Float
      saldoAtualizado = saldo + valor
  hClose saldoFile
  extratoFile <- openFile extratoPath AppendMode
  hPutStrLn extratoFile ("+" ++ show valor)
  hClose extratoFile
  saldoFile <- openFile saldoPath WriteMode
  hPutStrLn saldoFile (show saldoAtualizado)
  hClose saldoFile

saque :: Float -> IO ()
saque valor = do
  currentDir <- getCurrentDirectory
  let saldoPath = currentDir ++ "/saldo.txt"
      extratoPath = currentDir ++ "/extrato.txt"
  saldoFile <- openFile saldoPath ReadMode
  saldoStr <- hGetLine saldoFile
  let saldo = read saldoStr :: Float
  hClose saldoFile
  if valor <= saldo then do
    let saldoAtualizado = saldo - valor
    extratoFile <- openFile extratoPath AppendMode
    hPutStrLn extratoFile ("-" ++ show valor)
    hClose extratoFile
    saldoFile <- openFile saldoPath WriteMode
    hPutStrLn saldoFile (show saldoAtualizado)
    hClose saldoFile
  else
    putStrLn "Saldo insuficiente para realizar saque."

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "==============================Banco Renan=============================="
  putStrLn "Opções:"
  putStrLn "1. Saldo"
  putStrLn "2. Extrato"
  putStrLn "3. Depósito"
  putStrLn "4. Saque"
  putStrLn "5. Fim"
  putStrLn "Escolha uma opção:"
  opcao <- getLine
  case opcao of
    "1" -> do
      putStrLn "Saldo:"
      imprime "saldo.txt"
      main
    "2" -> do
      putStrLn "Extrato:"
      imprime "extrato.txt"
      main
    "3" -> do
      putStrLn "Digite o valor do depósito:"
      valor <- getLine
      deposito (read valor)
      main
    "4" -> do
      putStrLn "Digite o valor do saque:"
      valor <- getLine
      saque (read valor)
      main
    "5" -> do
      putStrLn "Obrigado por utilizar o Banco Renan!"
    _ -> do
      putStrLn "Opção inválida!"
      main
