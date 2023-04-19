import System.Exit (exitSuccess)
import System.IO

data Date = Date {day :: Int, month :: Int, year :: Int}

data Tarea = Tarea {descripcion :: String, estado :: String, fechaVencimiento :: Date}

rutaGuardado :: FilePath
rutaGuardado = "guardado/listaTareas.txt"

main = do
  cargar rutaGuardado

cargar :: FilePath -> IO ()
cargar ruta = do
  contenido <- readFile ruta
  putStrLn contenido
  menu

menu :: IO ()
menu = do
  putStrLn " "
  putStrLn "--------------------------"
  putStrLn "----  Menu Principal  ----"
  putStrLn "1. Agregar tarea"
  putStrLn "2. Editar tarea"
  putStrLn "3. Mostrar lista de tareas"
  putStrLn "4. Eliminar tarea"
  putStrLn "0. Guardar y salir"
  putStrLn "--------------------------"
  putStr "---- Opcion: "
  opcion <- getLine

  case opcion of
    "1" -> putStrLn "1 SELECCIONADO"
    "2" -> putStrLn "2 SELECCIONADO"
    "3" -> putStrLn "3 SELECCIONADO"
    "4" -> putStrLn "4 SELECCIONADO"
    "0" -> exitSuccess
    op -> putStrLn "Seleccione una opcion valida!"
  menu
