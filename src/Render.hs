module Render where

import Control.Exception
import Control.Monad
import Data.StateVar
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL
import qualified Graphics.UI.GLFW as GLFW
import Text.RawString.QQ
import qualified Graphics.Rendering.OpenGL.GLU.Errors as GLE

data RenderException
    = DrawerException String
    | GLException String [GLE.Error]
    | GLFWException String
    deriving Show

instance Exception RenderException

data Drawer
    = Drawer { drawer_vao :: VertexArrayObject
             , drawer_vbo :: BufferObject
             , drawer_program :: Program
             }

data Image
    = Image TextureObject

type DrwVertex = (GLfloat, GLfloat)
type DrwColor = (GLfloat, GLfloat, GLfloat, GLfloat)
type DrwTexcoord = (GLfloat, GLfloat)
type ScreenCoord = (GLfloat, GLfloat)

withGLFW :: IO a -> IO a
withGLFW action = do
    GLFW.setErrorCallback . Just $ \err str ->
        throwIO . GLFWException $ show err ++ ": " ++ str
    success <- GLFW.init
    unless success . throwIO . GLFWException $ "Unable to initialize GLFW."
    action `finally` GLFW.terminate

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO a) -> IO a
withWindow width height title action = do
    win <- GLFW.createWindow width height title Nothing Nothing
    case win of
        Nothing -> throwIO . GLFWException $ "Unable to create windows."
        Just w -> body w `finally` GLFW.destroyWindow w
    where body win = do
              (wid, hei) <- GLFW.getFramebufferSize win
              onResize win wid hei
              GLFW.setFramebufferSizeCallback win (Just onResize)
              action win
          onResize win wid hei = do
              GLFW.makeContextCurrent (Just win)
              viewport $= (Position 0 0, Size (fromIntegral wid) (fromIntegral hei))

createDrawer :: IO Drawer
createDrawer
    = withObjectName2 $ \vao vbo -> do
        bindVertexArrayObject $= Just vao
        bindBuffer ArrayBuffer $= Just vbo
        withProgram $ \prog -> do
            vpos <- get $ attribLocation prog "vpos"
            vcolor <- get $ attribLocation prog "vcolor"
            vtexcoord <- get $ attribLocation prog "vtexcoord"
            let stride = fromIntegral . glFloatSize $ 2 + 4 + 2
            vertexAttribPointer vpos $=
                (ToFloat, VertexArrayDescriptor 2 Float stride (glFloatOffset 0))
            vertexAttribArray vpos $= Enabled
            vertexAttribPointer vcolor $=
                (ToFloat, VertexArrayDescriptor 4 Float stride (glFloatOffset 2))
            vertexAttribArray vcolor $= Enabled
            vertexAttribPointer vtexcoord $=
                (ToFloat, VertexArrayDescriptor 2 Float stride (glFloatOffset 6))
            vertexAttribArray vtexcoord $= Enabled
            bindVertexArrayObject $= Nothing
            checkGLError
            return (Drawer vao vbo prog)

destroyDrawer :: Drawer -> IO ()
destroyDrawer (Drawer vao vbo prog) = do
    deleteObjectName vao
    deleteObjectName vbo
    deleteObjectName prog

createImage :: Integral a => a -> a -> [DrwColor] -> IO Image
createImage width height pixels
    = withObjectName $ \tex -> do
        activeTexture $= TextureUnit 0
        textureBinding Texture2D $= Just tex
        let buffer = concatMap (\(r, g, b, a) -> [r, g, b, a]) pixels
        withArray buffer $ \ptr ->
            texImage2D Texture2D NoProxy 0 RGBA' (TextureSize2D (fromIntegral width) (fromIntegral height))
                0 (PixelData RGBA Float ptr)
        -- generateMipmap' Texture2D
        -- textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
        -- textureWrapMode Texture2D S $= (Repeated, Repeat)
        -- textureWrapMode Texture2D T $= (Repeated, Repeat)
        let s = fromIntegral (width * height * 4) 
        ptr <- mallocArray s :: IO (Ptr Float)
        getTexImage Texture2D 0 (PixelData RGBA Float ptr)
        buf <- peekArray s ptr
        free ptr
        -- putStrLn . show . toInteger $ width
        -- putStrLn . show . toInteger $ height
        putStrLn . show $ buf == buffer
        putStrLn . show $ buf
        putStrLn . show $ buffer
        checkGLError
        return (Image tex)

destroyImage :: Image -> IO ()
destroyImage (Image tex) = deleteObjectName tex

drawColor :: Drawer -> PrimitiveMode -> [(DrwVertex, DrwColor)] -> IO ()
drawColor drawer mode vcs = do
    currentProgram $= Just (drawer_program drawer)
    enable_texture <- get $
        uniformLocation (drawer_program drawer) "enable_texture"
    tex <- get $
        uniformLocation (drawer_program drawer) "tex"
    uniform enable_texture $= (0 :: GLint)
    uniform tex $= TextureUnit 0
    bindVertexArrayObject $= Just (drawer_vao drawer)
    let buffer = concatMap (\(v, c) -> v2l v ++ c2l c ++ [0, 0]) vcs
        v2l (x, y) = [x, y]
        c2l (r, g, b, a) = [r, g, b, a]
    withArray buffer $ \ptr -> do
        let size = fromIntegral $ length buffer * sizeOf (1 :: GLfloat)
        bindBuffer ArrayBuffer $= Just (drawer_vbo drawer)
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)
    drawArrays mode 0 . fromIntegral $ length vcs
    bindVertexArrayObject $= Nothing
    checkGLError

drawImage :: Drawer -> Image -> (GLfloat, GLfloat) -> GLfloat -> GLfloat -> IO ()
drawImage drawer (Image image) (posX, posY) width height = do
    currentProgram $= Just (drawer_program drawer)
    bindVertexArrayObject $= Just (drawer_vao drawer)
    enable_texture <- get $
        uniformLocation (drawer_program drawer) "enable_texture"
    tex <- get $
        uniformLocation (drawer_program drawer) "tex"
    uniform enable_texture $= (1 :: GLint)
    uniform tex $= TextureUnit 0
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just image
    let buffer = [ posX        , posY         , 1, 1, 1, 1, 0, 0
                 , posX        , posY + height, 1, 1, 1, 1, 0, 1
                 , posX + width, posY         , 1, 1, 1, 1, 1, 0
                 , posX        , posY + height, 1, 1, 1, 1, 0, 1
                 , posX + width, posY         , 1, 1, 1, 1, 1, 0
                 , posX + width, posY + height, 1, 1, 1, 1, 1, 1
                 ]
    withArray buffer $ \ptr -> do
        let size = fromIntegral $ length buffer * sizeOf (1 :: GLfloat)
        bindBuffer ArrayBuffer $= Just (drawer_vbo drawer)
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)
    drawArrays Triangles 0 . fromIntegral $ length buffer
    bindVertexArrayObject $= Nothing
    checkGLError

withShader :: ShaderType -> String -> (Shader -> IO a) -> IO a
withShader ty source action = do
    createShader ty `bracket` deleteObjectName $ \shader -> do
        shaderSourceBS shader $= packUtf8 source
        compileShader shader
        stat <- compileStatus shader
        unless stat $
            shaderInfoLog shader >>= throwIO . DrawerException
        action shader

withProgram :: (Program -> IO a) -> IO a
withProgram action = do
    createProgram `bracketOnError` deleteObjectName $ \prog -> do
        withShader VertexShader vertexShaderSource $ \vs -> do
            attachShader prog vs
            withShader FragmentShader fragmentShaderSource $ \fs -> do
                attachShader prog fs
                linkProgram prog
                stat <- linkStatus prog
                unless stat $
                    programInfoLog prog >>= throwIO . DrawerException
                action prog

vertexShaderSource :: String
vertexShaderSource
    = [r| #version 330 core
          in vec2 vpos;
          in vec4 vcolor;
          in vec2 vtexcoord;
          out vec4 fcolor;
          out vec2 ftexcoord;
          void main() {
              gl_Position = vec4(vpos, 0, 1);
              fcolor = vcolor;
              ftexcoord = vtexcoord;
          }
    |]

fragmentShaderSource :: String
fragmentShaderSource
    = [r| #version 330 core
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

checkGLError :: IO ()
checkGLError = do
    es <- get GLE.errors
    when (not . null $ es) .
        throwIO . GLException "" $ es

withObjectName :: GeneratableObjectName a => (a -> IO b) -> IO b
withObjectName action = do
    obj <- genObjectName
    action obj `onException` deleteObjectName obj

withObjectName2 :: (GeneratableObjectName a, GeneratableObjectName b)
                => (a -> b -> IO c)
                -> IO c
withObjectName2 action
    = withObjectName $ \a ->
        withObjectName $ \b ->
            action a b
