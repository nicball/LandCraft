module Render where

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

data Image
    = Image TextureObject

type DrwVertex = (GLfloat, GLfloat)
type DrwColor = (GLfloat, GLfloat, GLfloat, GLfloat)
type DrwTexcoord = (GLfloat, GLfloat)

createDrawer :: IO Drawer
createDrawer = do
    vao <- genObjectName
    bindVertexArrayObject $= Just vao
    vbo <- genObjectName
    bindBuffer ArrayBuffer $= Just vbo
    prog <- loadProgram
    vpos <- get $ attribLocation prog "vpos"
    vcolor <- get $ attribLocation prog "vcolor"
    vtexcoord <- get $ attribLocation prog "vtexcoord"
    let stride = glFloatSize (2 + 4 + 2)
    vertexAttribPointer vpos $=
        (ToFloat, VertexArrayDescriptor 2 Float stride (glFloatOffset 0))
    vertexAttribPointer vcolor $=
        (ToFloat, VertexArrayDescriptor 4 Float stride (glFloatOffset 2))
    vertexAttribPointer vtexcoord $=
        (ToFloat, VertexArrayDescriptor 2 Float stride (glFloatOffset 6))
    bindVertexArrayObject $= Nothing
    return (Drawer vao prog)

closeDrawer :: Drawer -> IO ()
closeDrawer (Drawer vao prog) = do
    deleteObjectName vao
    deleteObjectName prog

createImage :: Integral a => a -> a -> [DrwColor] -> IO Image
createImage width height pixels = do
    tex <- genObjectName
    

drawColor :: Drawer -> PrimitiveMode -> [(DrwVertex, DrwColor)] -> IO ()
drawColor drawer mode vcs = do
    currentProgram $= Just (drawer_program drawer)
    enable_texture <- get $
        uniformLocation (drawer_program drawer) "enable_texture"
    tex <- get $
        uniformLocation (drawer_program drawer) "tex"
    uniform enable_texture $= False
    uniform tex $= TextureUnit 0
    let buffer = concatMap (\(v, c) -> v2l v ++ c2l c ++ [0, 0]) vcs
        v2l (x, y) = [x, y]
        c2l (r, g, b, a) = [r, g, b, a]
    bindVertexArrayObject $= Just (drawer_vao drawer)
    withArray buffer $ \ptr -> do
        let size = length buffer * sizeOf (1 :: GLfloat)
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)
    drawArrays mode 0 (length vcs)
    bindVertexArrayObject $= Nothing

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
         in vec2 vtexcoord;
         out vec4 fcolor;
         void main() {
             gl_Position = vpos;
             fcolor = vcolor;
             ftexcoord = vtexcoord;
         }
      |]

fragmentShaderSource :: String
fragmentShaderSource
    = [r|#version 330 core
         in vec4 fcolor;
         in vec2 ftexcoord;
         out vec4 color;
         uniform sampler2D tex;
         uniform bool enable_texture;
         void main() {
             if (enable_texture) color = texture(tex, ftexcoord);
             else color = fcolor;
         }
      |]

bufferOffset :: Int -> Ptr b
bufferOffset = plusPtr nullPtr

glFloatOffset :: Int -> Ptr b
glFloatOffset n = bufferOffset $ n * sizeOf (1 :: GLfloat)

glFloatSize :: Int -> Int
glFloatSize n = n * sizeOf (1 :: GLfloat)
