import System.Exit (exitSuccess)
import System.IO
import Data.Text (Text, pack, splitOn, unpack, append)
import System.Posix.Internals (puts)

-- data Fecha = Fecha {dia :: Int, mes :: Int, año :: Int}

data Tarea = Tarea {descripcion :: String, estado :: String, fechaVencimiento :: String}

rutaGuardado :: FilePath
rutaGuardado = "guardado/listaTareas.txt"

main = do
  cargar rutaGuardado

cargar :: FilePath -> IO ()
cargar ruta = do
  handler <- openFile ruta ReadMode
  contenido <- hGetContents handler
  let tareas = leerListaTareas (lines contenido)
  -- print (tareasAString tareas)
  menu tareas
  hClose handler

leerListaTareas :: [String] -> [Tarea]
leerListaTareas lineas = do
  let cabecera = head lineas
  let lineasSinCabecera = tail lineas
  let tareasPorConvertir = map separarEnLista lineasSinCabecera
  map transformarLinea tareasPorConvertir

separarEnLista linea = splitOn (pack ";") (pack linea)

transformarLinea :: [Text] -> Tarea
transformarLinea [a, b, c] = Tarea (unpack a) (unpack b) (unpack c)

tareasAString :: [Tarea] -> String
tareasAString [] = []
tareasAString (x : xs) = descripcion x ++ ";" ++ estado x ++ ";" ++ fechaVencimiento x ++ "\n" ++ tareasAString xs

menu :: [Tarea] -> IO ()
menu tareas = do
  putStrLn " "
  putStrLn "--------------------------"
  putStrLn "----  Menú Principal  ----"
  putStrLn "1. Agregar tarea"
  putStrLn "2. Editar tarea"
  putStrLn "3. Mostrar tareas"
  putStrLn "4. Eliminar tarea"
  putStrLn "5. Guardar"
  putStrLn "0. Salir"
  putStrLn "--------------------------"
  putStr "----  Opción: "
  opcion <- getLine

  case opcion of
    "1" -> opcion1_Agregar tareas
    "2" -> putStrLn "2 SELECCIONADO"
    "3" -> opcion3_submenuMostrar tareas
    "4" -> putStrLn "4 SELECCIONADO"
    "0" -> exitSuccess
    op -> putStrLn "Seleccione una opcion valida!"
  menu tareas

opcion1_Agregar :: [Tarea] -> IO ()
opcion1_Agregar tareas = do
  putStrLn "** AGREGAR TAREA **"
  putStr "Descripción: "
  descripcion <- getLine

  putStrLn "Estado:"
  putStrLn "** Introduzca un número"
  putStrLn "1. Pendiente"
  putStrLn "2. En proceso"
  putStrLn "3. Terminada"
  estado <- getEstado

  putStr "Fecha de vencimiento (aaaa/mm/dd): "
  fecha <- getLine
  let tarea = Tarea {descripcion = descripcion, estado = estado, fechaVencimiento = fecha}
  putStr ("Se registró exitosamente la tarea con descripción <" ++ descripcion ++ ">")
  menu (tarea : tareas)

getEstado :: IO String
getEstado = do
  putStr "Opción: "
  estado <- getLine
  
  case estado of
    "1" -> pure "Pendiente"
    "2" -> pure "En proceso"
    "3" -> pure "Terminado"
    op -> regresarGetEstado

regresarGetEstado = do
  putStrLn "Opción inválida!"
  getEstado

opcion3_submenuMostrar tareas = do
  let header = "Descripción;Estado;FechaVencimiento\n"
  putStrLn " "
  putStrLn "--------------------------"
  putStrLn "---  Tareas a mostrar  ---"
  putStrLn "1. Todas"
  putStrLn "2. Tareas pendientes"
  putStrLn "3. Tareas en proceso"
  putStrLn "4. Tareas terminadas"
  putStrLn "0. Volver al menú principal"
  putStrLn "--------------------------"
  putStr "---  Opción: "
  opcion <- getLine

  case opcion of
    "1" -> putStrLn (header ++ tareasAString tareas)
    "2" -> putStrLn (header ++ mostrarTareasPendientes tareas)
    "3" -> putStrLn (header ++ mostrarTareasEnProceso tareas)
    "4" -> putStrLn (header ++ mostrarTareasTerminadas tareas)
    "0" -> menu tareas
    op -> putStrLn "Seleccione una opción válida!"
  opcion3_submenuMostrar tareas

mostrarTareasPendientes :: [Tarea] -> String
mostrarTareasPendientes [] = []
mostrarTareasPendientes (x : xs)
  | estado x == "Pendiente" = descripcion x ++ ";" ++ estado x ++ ";" ++ fechaVencimiento x ++ "\n" ++ mostrarTareasPendientes xs
  | otherwise = mostrarTareasPendientes xs

mostrarTareasEnProceso :: [Tarea] -> String
mostrarTareasEnProceso [] = []
mostrarTareasEnProceso (x : xs)
  | estado x == "En proceso" = descripcion x ++ ";" ++ estado x ++ ";" ++ fechaVencimiento x ++ "\n" ++ mostrarTareasEnProceso xs
  | otherwise = mostrarTareasEnProceso xs

mostrarTareasTerminadas :: [Tarea] -> String
mostrarTareasTerminadas [] = []
mostrarTareasTerminadas (x : xs)
  | estado x == "Terminada" = descripcion x ++ ";" ++ estado x ++ ";" ++ fechaVencimiento x ++ "\n" ++ mostrarTareasTerminadas xs
  | otherwise = mostrarTareasTerminadas xs