open Tracer.Basics
open System.IO
open System.Numerics
open Transform
open Transformation

[<EntryPoint>]
let main _ = 
    
    let position = Point(0.,0.,8.)
    let lookat = Point(0.,0.,0.)
    let up = Vector(0.,1.,0.)
    let zoom = 1.
    let resX = 1920
    let resY = 1080
    let width = 2.
    let height = (float(resY) / float(resX)) * width
    
    //- MATERIALS
    let red = Textures.mkMatTexture(MatteMaterial(Colour.Red))
    let white = Textures.mkMatTexture(MatteMaterial(Colour.White))


    let zero = 0.
    let two = 2.
    let four = 4.
    let six = 6.
    let eight = 8.
    let ten = 10.
    let twelve = 12.
    let fourteen = 14.
    let sixteen = 16.

    //- SHAPES
    let spherere1 = SphereShape(Point(0.,0.,0.), 1., red)
    let spherere2 = SphereShape(Point(0.,0.,0.), 1., white)

    let sphere3 = transform spherere1 (translate -10. zero 0.)
    let sphere4 = transform spherere1 (translate -8. zero 0.)
    let sphere5 = transform spherere1 (translate -6. zero 0.)
    let sphere6 = transform spherere1 (translate -4. zero 0.)
    let sphere7 = transform spherere2 (translate -2. zero 0.)
    let sphere8 = transform spherere1 (translate 2. zero 0.)
    let sphere9 = transform spherere1 (translate 4. zero 0.)
    let sphere10 = transform spherere1 (translate 6. zero 0.)
    let sphere11 = transform spherere1 (translate 8. zero 0.)
    let sphere12= transform spherere1 (translate 10. zero 0.)

    let sphere13 = transform spherere1 (translate -10. two 0.)
    let sphere14 = transform spherere1 (translate -8. two 0.)
    let sphere15 = transform spherere1 (translate -6. two 0.)
    let sphere16 = transform spherere1 (translate -4. two 0.)
    let sphere17 = transform spherere2 (translate -2. two 0.)
    let sphere18 = transform spherere1 (translate 2. two 0.)
    let sphere19 = transform spherere1 (translate 4. two 0.)
    let sphere20 = transform spherere1 (translate 6. two 0.)
    let sphere21 = transform spherere1 (translate 8. two 0.)
    let sphere22= transform spherere1 (translate 10. two 0.)

    let sphere23 = transform spherere1 (translate -10. four 0.)
    let sphere24 = transform spherere1 (translate -8. four 0.)
    let sphere25 = transform spherere1 (translate -6. four 0.)
    let sphere26 = transform spherere1 (translate -4. four 0.)
    let sphere27 = transform spherere2 (translate -2. four 0.)
    let sphere28 = transform spherere1 (translate 2. four 0.)
    let sphere29 = transform spherere1 (translate 4. four 0.)
    let sphere30 = transform spherere1 (translate 6. four 0.)
    let sphere31 = transform spherere1 (translate 8. four 0.)
    let sphere32= transform spherere1 (translate 10. four 0.)

    let sphere33 = transform spherere1 (translate -10. six 0.)
    let sphere34 = transform spherere1 (translate -8. six 0.)
    let sphere35 = transform spherere1 (translate -6. six 0.)
    let sphere36 = transform spherere1 (translate -4. six 0.)
    let sphere37 = transform spherere2 (translate -2. six 0.)
    let sphere38 = transform spherere1 (translate 2. six 0.)
    let sphere39 = transform spherere1 (translate 4. six 0.)
    let sphere40 = transform spherere1 (translate 6. six 0.)
    let sphere41 = transform spherere1 (translate 8. six 0.)
    let sphere42= transform spherere1 (translate 10. six 0.)

    let sphere43 = transform spherere1 (translate -10. eight 0.)
    let sphere44 = transform spherere1 (translate -8. eight 0.)
    let sphere45 = transform spherere1 (translate -6. eight 0.)
    let sphere46 = transform spherere1 (translate -4. eight 0.)
    let sphere47 = transform spherere2 (translate -2. eight 0.)
    let sphere48 = transform spherere1 (translate 2. eight 0.)
    let sphere49 = transform spherere1 (translate 4. eight 0.)
    let sphere50 = transform spherere1 (translate 6. eight 0.)
    let sphere51 = transform spherere1 (translate 8. eight 0.)
    let sphere52= transform spherere1 (translate 10. eight 0.)

    let sphere63 = transform spherere1 (translate -10. ten 0.)
    let sphere64 = transform spherere1 (translate -8. ten 0.)
    let sphere65 = transform spherere1 (translate -6. ten 0.)
    let sphere66 = transform spherere1 (translate -4. ten 0.)
    let sphere67 = transform spherere2 (translate -2. ten 0.)
    let sphere68 = transform spherere1 (translate 2. ten 0.)
    let sphere69 = transform spherere1 (translate 4. ten 0.)
    let sphere70 = transform spherere1 (translate 6. ten 0.)
    let sphere71 = transform spherere1 (translate 8. ten 0.)
    let sphere72= transform spherere1 (translate 10. ten 0.)

    let sphere73 = transform spherere1 (translate -10. twelve 0.)
    let sphere74 = transform spherere1 (translate -8. twelve 0.)
    let sphere75 = transform spherere1 (translate -6. twelve 0.)
    let sphere76 = transform spherere1 (translate -4. twelve 0.)
    let sphere77 = transform spherere2 (translate -2. twelve 0.)
    let sphere78 = transform spherere1 (translate 2. twelve 0.)
    let sphere79 = transform spherere1 (translate 4. twelve 0.)
    let sphere80 = transform spherere1 (translate 6. twelve 0.)
    let sphere81 = transform spherere1 (translate 8. twelve 0.)
    let sphere82= transform spherere1 (translate 10. twelve 0.)

    let sphere83 = transform spherere1 (translate -10. fourteen 0.)
    let sphere84 = transform spherere1 (translate -8. fourteen 0.)
    let sphere85 = transform spherere1 (translate -6. fourteen 0.)
    let sphere86 = transform spherere1 (translate -4. fourteen 0.)
    let sphere87 = transform spherere2 (translate -2. fourteen 0.)
    let sphere88 = transform spherere1 (translate 2. fourteen 0.)
    let sphere89 = transform spherere1 (translate 4. fourteen 0.)
    let sphere90 = transform spherere1 (translate 6. fourteen 0.)
    let sphere91 = transform spherere1 (translate 8. fourteen 0.)
    let sphere92= transform spherere1 (translate 10. fourteen 0.)

    let sphere93 = transform spherere1 (translate -10. sixteen 0.)
    let sphere94 = transform spherere1 (translate -8. sixteen 0.)
    let sphere95 = transform spherere1 (translate -6. sixteen 0.)
    let sphere96 = transform spherere1 (translate -4. sixteen 0.)
    let sphere97 = transform spherere2 (translate -2. sixteen 0.)
    let sphere98 = transform spherere1 (translate 2. sixteen 0.)
    let sphere99 = transform spherere1 (translate 4. sixteen 0.)
    let sphere100 = transform spherere1 (translate 6. sixteen 0.)
    let sphere101 = transform spherere1 (translate 8. sixteen 0.)
    let sphere102= transform spherere1 (translate 10. sixteen 0.)

    let sphere53 = transform spherere1 (translate 0. two 0.)
    let sphere54 = transform spherere1 (translate 0. four 0.)
    let sphere55 = transform spherere1 (translate 0. six 0.)
    let sphere56 = transform spherere1 (translate 0. eight 0.)
    let sphere57 = transform spherere1 (translate 0. ten 0.)
    let sphere58 = transform spherere1 (translate 0. twelve 0.)
    let sphere59 = transform spherere1 (translate 0. fourteen 0.)
    let sphere60 = transform spherere1 (translate 0. sixteen 0.)

    let testList:list<Shape> = [spherere1; sphere102; sphere3; sphere4; sphere5; 
                                sphere6; sphere7; sphere8; sphere9; sphere10; 
                                sphere11; sphere12; sphere13; sphere14; sphere15; 
                                sphere16; sphere17; sphere18; sphere19; sphere20; 
                                sphere21; sphere22; sphere23; sphere24; sphere25; 
                                sphere26; sphere27; sphere28; sphere29; sphere30; 
                                sphere31; sphere32; sphere33; sphere34; sphere35;
                                sphere36; sphere37; sphere38; sphere39; sphere40; 
                                sphere41; sphere42; sphere43; sphere44; sphere45; 
                                sphere46; sphere47; sphere48; sphere49; sphere50; 
                                sphere51; sphere52; sphere53; sphere54; sphere55; 
                                sphere56; sphere57; sphere58; sphere59; sphere60; 
                                sphere63; sphere64; sphere65; 
                                sphere66; sphere67; sphere68; sphere69; sphere70;
                                sphere71; sphere72; sphere73; sphere74; sphere75; 
                                sphere76; sphere77; sphere78; sphere79; sphere80; 
                                sphere81; sphere82; sphere83; sphere84; sphere85; 
                                sphere86; sphere87; sphere88; sphere89; sphere90; 
                                sphere91; sphere92; sphere93; sphere94; sphere95; 
                                sphere96; sphere97; sphere98; sphere99; sphere100; sphere101]

    
    //- THIN LENS SAMPLE SETTINGS
    let CAM_SETS = 129
    let VIEW_SAMPLES = 8
    let LENS_SAMPLES = 8
    
    //- LIGHTS
    let directional = DirectionalLight(Colour.White, 1.5, Vector(1., 1., 1.))
    let lightAmbient   = AmbientLight(Colour.White, 0.1)

    //- FINAL
    let camera                  = PinholeCamera(position, lookat, up, zoom, width, height, resX, resY)
    let lights: Light list      = [directional; lightAmbient]
    //let spheres : Shape array   = [| spherere1; spherere2 |]
    //let spheres: Shape array    = [| spherere1; sphere53; sphere54; sphere55; sphere56; sphere57; sphere58; sphere59; sphere60 |]
    let spheres: Shape array    = List.toArray testList
    //let spheres: Shape array    = List.toArray testList2
    let scene                   = Scene(spheres, camera, lights)

    let kdTree = Acceleration.KD_tree.buildKDTree spheres

    //printfn "%A" kdTree

    ignore (scene.Render)
    
    0