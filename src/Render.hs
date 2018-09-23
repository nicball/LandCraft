module Render

import Control.Exception
import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.UI.GLUT
import Text.RawString.QQ

data DrawerException
    = DrawerException String
    deriving Show

instance Exception DrawerException

data Drawer
    = Drawer { drawer_vao :: VertexArrayObject
             , drawer_program :: Program
             }

type DrwVertex = (GLfloat, GLfloat)
type DrwColor = (GLfloat, GLfloat, GLfloat, GLfloat)

createDrawer :: IO Drawer
createDrawer = do
    vao <- genObjectName
    bindVertexArrayObject $= Just vao
    vbo <- genObjectName
    bindBuffer ArrayBuffer $= Just vbo
    prog <- loadProgram
    vpos <- get $ attribLocation prog "vpos"
    vcolor <- get $ attribLocation prog "vcolor"
    vertexAttribPointer vpos $=
        (ToFloat, VertexArrayDescriptor 2 Float 24 (glFloatOffset 0))
    vertexAttribPointer vcolor $=
        (ToFloat, VertexArrayDescriptor 4 Float 24 (glFloatOffset 2))
    bindVertexArrayObject $= Nothing
    return (Drawer vao prog)

draw :: PrimitiveMode -> [(DrwVertex, DrwColor)] -> Drawer -> IO ()
draw mode vcs drawer = do
    bindVertexArrayObject $= Just (drawer_vao drawer)
    currentProgram $= Just (drawer_program drawer)
    let buffer = concatMap (\(v, c) -> v2l v ++ c2l c) vcs
        v2l (x, y) = [x, y]
        c2l (r, g, b, a) = [r, g, b, a]
    withArray buffer $ \ptr -> do
        let size = length buffer * sizeOf (1 :: GLfloat)
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)
    drawArrays mode 0 (length vcs)

loadShader :: ShaderType -> String -> IO Shader
loadShader ty source = do
    createShader ty `bracketOnError` deleteObjectName $ \shader -> do
        shaderSourceBS shader $= packUtf8 source
        compileShader shader
        stat <- compileStatus shader
        unless stat $
            shaderInfoLog shader >>= throwIO . DrawerException

loadProgram :: IO Program
loadProgram = do
    createProgram `bracketOnError` deleteObjectName $ \prog -> do
        loadShader VertexShader vertexShaderSource
            >>= attachShader prog
        loadShader FragmentShader fragmentShaderSource
            >>= attachShader prog
        linkProgram prog
        stat <- linkStatus prog
        unless stat $
            programInfoLog prog >>= throwIO . DrawerException

vertexShaderSource :: String
vertexShaderSource
    = [r|#version330 core
         in vec2 vpos;
         in vec4 vcolor;
         out vec4 fcolor;
         void main() {
             gl_Position = vpos;
             fcolor = vcolor;
         }
      |]

fragmentShaderSource :: String
fragmentShaderSource
    = [r|#version 330 core
         in vec4 fcolor;
         out vec4 color;
         void main() {
             color = fcolor;
         }
      |]

bufferOffset :: Num a => a -> Ptr b
bufferOffset = plusPtr nullPtr

glFloatOffset :: Num a => a -> Ptr b
glFloatOffset n = bufferOffset $ n * sizeOf (1 :: GLfloat)
