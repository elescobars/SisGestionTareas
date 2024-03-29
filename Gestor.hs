-- @autores
--   Bon Cabanillas Rebeca Sofía
--   Escobar Sánchez José Alejandro
-- @proyecto: 2 - Sistema de gestión de tareas
-- @repo: https://github.com/elescobars/SisGestionTareas

import System.Exit (exitSuccess)
import System.IO (hClose, hGetContents, openFile, IOMode(ReadMode))
import Data.Text (Text, pack, splitOn, unpack)

data Tarea = Tarea {descripcion :: String, estado :: String, fechaVencimiento :: String}

-- Ubicación del archivo que manipula todas las tareas
rutaGuardado :: FilePath
rutaGuardado = "guardado/listaTareas.txt"

-- Módulo principal a invocar en ejecución
main :: IO ()
main = do
  cargar rutaGuardado

-- Lectura del archivo, para recuperar su contenido
cargar :: FilePath -> IO ()
cargar ruta = do
  handler <- openFile ruta ReadMode
  contenido <- hGetContents handler
  let tareas = leerListaTareas (lines contenido)
  menu tareas
  hClose handler

leerListaTareas :: [String] -> [Tarea]
leerListaTareas lineas = do
  let cabecera = head lineas
  let lineasSinCabecera = tail lineas
  let tareasPorConvertir = map separarEnLista lineasSinCabecera
  map transformarLinea tareasPorConvertir

separarEnLista :: String -> [Text]
separarEnLista linea = splitOn (pack ";") (pack linea)

transformarLinea :: [Text] -> Tarea
transformarLinea [a, b, c] = Tarea (unpack a) (unpack b) (unpack c)

-- Formato en el que se escriben las tareas en el archivo de texto, separando campos mediante ';'
tareasAString :: [Tarea] -> String
tareasAString [] = []
tareasAString (x : xs) = descripcion x ++ ";" ++ estado x ++ ";" ++ fechaVencimiento x ++ "\n" ++ tareasAString xs

unaTareaAString :: Tarea -> String
unaTareaAString x = descripcion x ++ ";" ++ estado x ++ ";" ++ fechaVencimiento x

-- Menú principal, se manejan localmente los cambios en la lista de tipo Tarea
-- Cuando el usuario decida grabarlos en el archivo de texto, selecciona la opción 5
menu :: [Tarea] -> IO ()
menu tareas = do
  putStrLn "--------------------------------"
  putStrLn "** MENU PRINCIPAL             **"
  putStrLn "--------------------------------"
  putStrLn "1. Agregar tarea"
  putStrLn "2. Editar tarea"
  putStrLn "3. Mostrar tareas"
  putStrLn "4. Eliminar tarea"
  putStrLn "5. Guardar"
  putStrLn "0. Salir"
  putStrLn "--------------------------------"
  putStr "-- Opción: "
  opcion <- getLine
  case opcion of
    "1" -> opcion1_Agregar tareas
    "2" -> opcion2_Editar tareas
    "3" -> opcion3_submenuMostrar tareas
    "4" -> opcion4_Eliminar tareas
    "5" -> opcion5_Guardar tareas
    "0" -> exitSuccess
    op -> putStrLn "!! ERROR: ¡Seleccione una opción válida!"
  menu tareas

opcion1_Agregar :: [Tarea] -> IO ()
opcion1_Agregar tareas = do
  putStrLn "--------------------------------"
  putStrLn "** AGREGAR TAREA              **"
  putStrLn "--------------------------------"
  putStr "Descripción: "
  descripcion <- getLine
  putStrLn "Estado:"
  putStrLn "** Introduzca un número"
  putStrLn "1. Pendiente"
  putStrLn "2. En proceso"
  putStrLn "3. Terminada"
  -- Validación de entrada correcta para el estado (que sea una opción que exista)
  estado <- inputEstado
  putStr "Fecha de vencimiento (aaaa/mm/dd): "
  fecha <- getLine
  let tarea = Tarea {descripcion = descripcion, estado = estado, fechaVencimiento = fecha}
  putStrLn ("** Se registró exitosamente la tarea con descripción: " ++ descripcion)
  -- Devuelve al menú la versión actualizada de la lista 'tareas' para continuar las tareas
  menu (tarea : tareas)

opcion2_Editar :: [Tarea] -> IO ()
opcion2_Editar tareas = do
  putStrLn "--------------------------------"
  putStrLn "** EDITAR TAREA               **"
  putStrLn "--------------------------------"
  putStrLn "** Lista de tareas existentes **"
  putStrLn (take (length (mostrarTareas tareas 1) - 1) (mostrarTareas tareas 1))
  putStrLn "--------------------------------"
  -- Validación de entrada correcta para el índice (que sea una tarea que exista)
  input <- inputIndex (length tareas)
  let index = read input - 1 :: Int
  putStrLn ("Editando la tarea " ++ input ++ " con descripción: " ++ descripcion (tareas !! index))
  putStrLn "--------------------------------"
  putStr "Nueva descripción: "
  descripcion <- getLine
  putStrLn "Nuevo estado:"
  putStrLn "** Introduzca un número"
  putStrLn "1. Pendiente"
  putStrLn "2. En proceso"
  putStrLn "3. Terminada"
  -- Validación de entrada correcta para el estado (que sea una opción que exista)
  estado <- inputEstado
  putStr "Nueva fecha de vencimiento (aaaa/mm/dd): "
  fecha <- getLine
  let tarea = Tarea {descripcion = descripcion, estado = estado, fechaVencimiento = fecha}
  -- take:    elementos en la lista anteriores a la tarea que se editó
  -- [tarea]: elemento que se editó
  -- drop:    elementos en la lista posteriores a la tarea que se editó
  -- nuevasTareas es el resultado de mantener todas las tareas excepto la que se modificó, sustituyéndola por su nueva versión, sin alterar el orden original de las tareas
  let nuevasTareas = take index tareas ++ [tarea] ++ drop (index + 1) tareas
  putStrLn "--------------------------------"
  putStrLn ("** Se editó exitosamente la tarea " ++ input ++ " con la nueva descripción: " ++ descripcion)
  -- Devuelve al menú la versión actualizada de la lista 'tareas' para continuar las tareas
  menu nuevasTareas
  
opcion3_submenuMostrar :: [Tarea] -> IO ()
opcion3_submenuMostrar tareas = do
  putStrLn "--------------------------------"
  putStrLn "** TAREAS PARA MOSTRAR        **"
  putStrLn "--------------------------------"
  putStrLn "1. Todas"
  putStrLn "2. Tareas pendientes"
  putStrLn "3. Tareas en proceso"
  putStrLn "4. Tareas terminadas"
  putStrLn "0. Volver al menú principal"
  putStrLn "--------------------------------"
  putStr "-- Opción: "
  opcion <- getLine
  putStrLn "--------------------------------"
  let header = "Num.;Descripción;Estado;FechaVencimiento\n"
  case opcion of
    -- Los métodos de tipo 'mostrarTareas[...]' concatenan al final un caracter de salto de línea '\n'
    -- La sentencia (take (length) ... ) suprime el último caracter no deseado
    "1" -> putStrLn (take (length (header ++ mostrarTareas tareas 1) - 1) (header ++ mostrarTareas tareas 1))
    "2" -> putStrLn (take (length (header ++ mostrarTareasPendientes tareas 1) - 1) (header ++ mostrarTareasPendientes tareas 1))
    "3" -> putStrLn (take (length (header ++ mostrarTareasEnProceso tareas 1) - 1) (header ++ mostrarTareasEnProceso tareas 1))
    "4" -> putStrLn (take (length (header ++ mostrarTareasTerminadas tareas 1) - 1) (header ++ mostrarTareasTerminadas tareas 1))
    "0" -> menu tareas
    op -> putStrLn "!! ERROR: ¡Seleccione una opción válida!"
  opcion3_submenuMostrar tareas

-- Los métodos de tipo 'mostrarTareas[...]' reciben la lista de tareas y un contador Int que les indica a partir de cuál número se desean crear los índices
mostrarTareas :: [Tarea] -> Int -> String
mostrarTareas [] _ = []
mostrarTareas (x : xs) contador = show contador ++ ";" ++ descripcion x ++ ";" ++ estado x ++ ";" ++ fechaVencimiento x ++ "\n" ++ mostrarTareas xs (contador + 1)

mostrarTareasPendientes :: [Tarea] -> Int -> String
mostrarTareasPendientes [] _ = []
mostrarTareasPendientes (x : xs) contador
  | estado x == "Pendiente" = show contador ++ ";" ++ descripcion x ++ ";" ++ estado x ++ ";" ++ fechaVencimiento x ++ "\n" ++ mostrarTareasPendientes xs (contador + 1)
  | otherwise = mostrarTareasPendientes xs contador

mostrarTareasEnProceso :: [Tarea] -> Int -> String
mostrarTareasEnProceso [] _ = []
mostrarTareasEnProceso (x : xs) contador
  | estado x == "En proceso" = show contador ++ ";" ++ descripcion x ++ ";" ++ estado x ++ ";" ++ fechaVencimiento x ++ "\n" ++ mostrarTareasEnProceso xs (contador + 1)
  | otherwise = mostrarTareasEnProceso xs contador

mostrarTareasTerminadas :: [Tarea] -> Int -> String
mostrarTareasTerminadas [] _ = []
mostrarTareasTerminadas (x : xs) contador
  | estado x == "Terminada" = show contador ++ ";" ++ descripcion x ++ ";" ++ estado x ++ ";" ++ fechaVencimiento x ++ "\n" ++ mostrarTareasTerminadas xs (contador + 1)
  | otherwise = mostrarTareasTerminadas xs contador

opcion4_Eliminar :: [Tarea] -> IO ()
opcion4_Eliminar tareas = do
  putStrLn "--------------------------------"
  putStrLn "** ELIMINAR TAREA             **"
  putStrLn "--------------------------------"
  putStrLn "** Lista de tareas existentes **"
  -- Omisión del último caracter no deseado: salto de línea '\n'
  putStrLn (take (length (mostrarTareas tareas 1) - 1) (mostrarTareas tareas 1))
  putStrLn "--------------------------------"
  -- Validación de entrada correcta para el índice (que sea una tarea que exista)
  input <- inputIndex (length tareas)
  let index = read input - 1 :: Int
  -- take:    elementos en la lista anteriores a la tarea que se editó
  -- drop:    elementos en la lista posteriores a la tarea que se editó
  -- nuevasTareas es el resultado de mantener todas las tareas excepto la que se eliminó, omitiéndola a propósito, sin alterar el orden original de las tareas
  let nuevasTareas = take index tareas ++ drop (index + 1) tareas
  putStrLn "--------------------------------"
  putStrLn ("** Se eliminó exitosamente la tarea " ++ input ++ " con descripción: " ++ descripcion (tareas !! index))
  -- Devuelve al menú la versión actualizada de la lista 'tareas' para continuar las tareas
  menu nuevasTareas

opcion5_Guardar :: [Tarea] -> IO ()
opcion5_Guardar tareas = do
  putStrLn "--------------------------------"
  putStrLn "Guardando archivo..."
  let header = "Descripción;Estado;FechaVencimiento\n"
  writeFile rutaGuardado (header ++ tareasAString tareas)
  putStrLn ("** ¡Archivo '" ++ rutaGuardado ++ "' guardado exitosamente!")
  menu tareas

-- El ciclo se controla con el módulo 'regresarInputEstado'
inputEstado :: IO String
inputEstado = do
  putStr "-- Opción: "
  estado <- getLine
  case estado of
    "1" -> pure "Pendiente"
    "2" -> pure "En proceso"
    "3" -> pure "Terminada"
    op -> regresarInputEstado

regresarInputEstado :: IO String
regresarInputEstado = do
  putStrLn "!! ERROR: ¡Opción inválida!"
  inputEstado

-- El ciclo se controla con el módulo 'regresarInputIndex'
-- 'inputIndex' devuelve el índice como lo escribió el usuario, comenzando con el índice 1. El sobrante se compensa en su aplicación con la llamada del módulo
inputIndex :: Int -> IO String
inputIndex totalTareas = do
  putStr "-- Introduzca el número de la tarea: "
  input <- getLine
  if read input > 0 && read input <= totalTareas then pure input else regresarInputIndex totalTareas

regresarInputIndex :: Int -> IO String
regresarInputIndex totalTareas = do
  putStrLn "!! ERROR: ¡No existe tarea asociada a este número!"
  inputIndex totalTareas