module ShapeTest

open Tracer.BaseShape
open Assert
open Tracer.Basics
open System


let allTest = 
    let rectangle = new Rectangle(Point(0.,0.,0.), Point(0.,1.,0.), Point(1.,0.,0.), Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.))))
    let disc = new Disc(Point(0.,0.,0.), 2., Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.))))
    let triangle = new Triangle(Point(0.,0.,0.), Point(0.,1.,0.), Point(1.,0.,0.), MatteMaterial(Colour(1.,1.,1.)))
    let sphere = new SphereShape(Point(0.,0.,0.), 2., Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.))))
    let hollowCylinder = new HollowCylinder(Point(0.,0.,0.), 2., 4., Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.))))
    let solidCylinder = new SolidCylinder(Point(0.,0.,0.), 2., 4., Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.))),
                                                                   Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.))),
                                                                   Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.))))
    let box = new Box(Point(0.,0.,0.), Point(1.,1.,1.), Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.))), 
                                                        Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.))), 
                                                        Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.))), 
                                                        Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.))), 
                                                        Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.))), 
                                                        Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.))))
    let plane = new InfinitePlane(Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.))))

    let baseRectangle = new BaseRectangle(Point(0.,0.,0.), Point(0.,1.,0.), Point(1.,0.,0.))
    let baseDisc = new BaseDisc(Point(0.,0.,0.), 2.)
    let baseTriangle = new BaseTriangle(Point(0.,0.,0.), Point(0.,1.,0.), Point(1.,0.,0.))
    let baseSphere = new BaseSphere(Point(0.,0.,0.), 2.)
    let baseHollowCylinder = new BaseHollowCylinder(Point(0.,0.,0.), 2., 4.)


    //Tests that shapes are built correctly
    Assert.Equal(rectangle.bottomleft, Point(0.,0.,0.), "rectangle.bottomLeft")
    Assert.Equal(rectangle.topLeft, Point(0.,1.,0.), "rectangle.topLeft")
    Assert.Equal(rectangle.bottomRight, Point(1.,0.,0.), "rectangle.bottomRight")

    Assert.Equal(disc.center, Point(0.,0.,0.), "disc.center")
    Assert.Equal(disc.radius, 2., "disc.radius")

    Assert.Equal(triangle.a, Point(0.,0.,0.), "triangle.a")
    Assert.Equal(triangle.b, Point(0.,1.,0.), "triangle.b")
    Assert.Equal(triangle.c, Point(1.,0.,0.), "triangle.c")

    Assert.Equal(sphere.Origin, Point(0.,0.,0.), "sphere.origin")
    Assert.Equal(sphere.Radius, 2., "sphere.radius")

    Assert.Equal(hollowCylinder.center, Point(0.,0.,0.), "hollowCylinder.center")
    Assert.Equal(hollowCylinder.radius, 2., "hollowCylinder.radius")
    Assert.Equal(hollowCylinder.height, 4., "hollowCylinder.height")

    Assert.Equal(solidCylinder.center, Point(0.,0.,0.), "solidCylinder.center")
    Assert.Equal(solidCylinder.radius, 2., "solidCylinder.radius")
    Assert.Equal(solidCylinder.height, 4., "solidCylinder.height")

    Assert.Equal(box.low, Point(0.,0.,0.), "box.low")
    Assert.Equal(box.low, Point(0.,0.,0.), "box.high")

    
    //Tests that BaseShapes are built correctly
    Assert.Equal(baseRectangle.bottomLeft, Point(0.,0.,0.), "baseRectangle.bottomLeft")
    Assert.Equal(baseRectangle.topLeft, Point(0.,1.,0.), "baseRectangle.topLeft")
    Assert.Equal(baseRectangle.bottomRight, Point(1.,0.,0.), "baseRectangle.bottomRight")

    Assert.Equal(baseDisc.center, Point(0.,0.,0.), "baseDisc.center")
    Assert.Equal(baseDisc.radius, 2., "baseDisc.radius")

    Assert.Equal(baseTriangle.a, Point(0.,0.,0.), "baseTriangle.a")
    Assert.Equal(baseTriangle.b, Point(0.,1.,0.), "baseTriangle.b")
    Assert.Equal(baseTriangle.c, Point(1.,0.,0.), "baseTriangle.c")

    Assert.Equal(baseSphere.origin, Point(0.,0.,0.), "baseSphere.origin")
    Assert.Equal(baseSphere.radius, 2., "baseSphere.radius")

    Assert.Equal(baseHollowCylinder.center, Point(0.,0.,0.), "baseHollowCylinder.center")
    Assert.Equal(baseHollowCylinder.radius, 2., "baseHollowCylinder.radius")
    Assert.Equal(baseHollowCylinder.height, 4., "baseHollowCylinder.height")

    //Tests for Bounding boxes being correctly returned with, getBoundingBox()
    let recBBox = rectangle.getBoundingBox ()
    Assert.Equal(recBBox.lowPoint.X, -0.000001, "test for rectangle Bounding box, lowPoint.X")
    Assert.Equal(recBBox.lowPoint.Y, -0.000001, "test for rectangle Bounding box, lowPoint.Y")
    Assert.Equal(recBBox.lowPoint.Z, -0.000001, "test for rectangle Bounding box, lowPoint.Z")
    Assert.Equal(recBBox.highPoint.X, 1.000001, "test for rectangle Bounding box, highPoint.X")
    Assert.Equal(recBBox.highPoint.Y, 1.000001, "test for rectangle Bounding box, highPoint.Y")
    Assert.Equal(recBBox.highPoint.Z, 0.000001, "test for rectangle Bounding box, highPoint.Z")

    let discBBox = disc.getBoundingBox ()
    Assert.Equal(discBBox.lowPoint.X, -2.000001, "test for disc Bounding box, lowPoint.X")
    Assert.Equal(discBBox.lowPoint.Y, -2.000001, "test for disc Bounding box, lowPoint.Y")
    Assert.Equal(discBBox.lowPoint.Z, -0.000001, "test for disc Bounding box, lowPoint.Z")
    Assert.Equal(discBBox.highPoint.X, 2.000001, "test for disc Bounding box, highPoint.X")
    Assert.Equal(discBBox.highPoint.Y, 2.000001, "test for disc Bounding box, highPoint.Y")
    Assert.Equal(discBBox.highPoint.Z, 0.000001, "test for disc Bounding box, highPoint.Z")

    let triBBox = triangle.getBoundingBox ()
    Assert.Equal(triBBox.lowPoint.X, -0.000001, "test for triangle Bounding box, lowPoint.X")
    Assert.Equal(triBBox.lowPoint.Y, -0.000001, "test for triangle Bounding box, lowPoint.Y")
    Assert.Equal(triBBox.lowPoint.Z, -0.000001, "test for triangle Bounding box, lowPoint.Z")
    Assert.Equal(triBBox.highPoint.X, 1.000001, "test for triangle Bounding box, highPoint.X")
    Assert.Equal(triBBox.highPoint.Y, 1.000001, "test for triangle Bounding box, highPoint.Y")
    Assert.Equal(triBBox.highPoint.Z, 0.000001, "test for triangle Bounding box, highPoint.Z")

    let sphereBBox = sphere.getBoundingBox ()
    Assert.Equal(sphereBBox.lowPoint.X, -2.000001, "test for sphere Bounding box, lowPoint.X")
    Assert.Equal(sphereBBox.lowPoint.Y, -2.000001, "test for sphere Bounding box, lowPoint.Y")
    Assert.Equal(sphereBBox.lowPoint.Z, -2.000001, "test for sphere Bounding box, lowPoint.Z")
    Assert.Equal(sphereBBox.highPoint.X, 2.000001, "test for sphere Bounding box, highPoint.X")
    Assert.Equal(sphereBBox.highPoint.Y, 2.000001, "test for sphere Bounding box, highPoint.Y")
    Assert.Equal(sphereBBox.highPoint.Z, 2.000001, "test for sphere Bounding box, highPoint.Z")

    let hollowBBox = hollowCylinder.getBoundingBox ()
    Assert.Equal(hollowBBox.lowPoint.X, -2.000001, "test for hollowCylinder Bounding box, lowPoint.X")
    Assert.Equal(hollowBBox.lowPoint.Y, -2.000001, "test for hollowCylinder Bounding box, lowPoint.Y")
    Assert.Equal(hollowBBox.lowPoint.Z, -2.000001, "test for hollowCylinder Bounding box, lowPoint.Z")
    Assert.Equal(hollowBBox.highPoint.X, 2.000001, "test for hollowCylinder Bounding box, highPoint.X")
    Assert.Equal(hollowBBox.highPoint.Y, 2.000001, "test for hollowCylinder Bounding box, highPoint.Y")
    Assert.Equal(hollowBBox.highPoint.Z, 2.000001, "test for hollowCylinder Bounding box, highPoint.Z")

    let solidBBox = solidCylinder.getBoundingBox ()
    Assert.Equal(solidBBox.lowPoint.X, -2.000001, "test for solidCylinder Bounding box, lowPoint.X")
    Assert.Equal(solidBBox.lowPoint.Y, -2.000001, "test for solidCylinder Bounding box, lowPoint.Y")
    Assert.Equal(solidBBox.lowPoint.Z, -2.000001, "test for solidCylinder Bounding box, lowPoint.Z")
    Assert.Equal(solidBBox.highPoint.X, 2.000001, "test for solidCylinder Bounding box, highPoint.X")
    Assert.Equal(solidBBox.highPoint.Y, 2.000001, "test for solidCylinder Bounding box, highPoint.Y")
    Assert.Equal(solidBBox.highPoint.Z, 2.000001, "test for solidCylinder Bounding box, highPoint.Z")

    let boxBBox = box.getBoundingBox ()
    Assert.Equal(boxBBox.lowPoint.X, -0.000001, "test for box Bounding box, lowPoint.X")
    Assert.Equal(boxBBox.lowPoint.Y, -0.000001, "test for box Bounding box, lowPoint.Y")
    Assert.Equal(boxBBox.lowPoint.Z, -0.000001, "test for box Bounding box, lowPoint.Z")
    Assert.Equal(boxBBox.highPoint.X, 1.000001, "test for box Bounding box, highPoint.X")
    Assert.Equal(boxBBox.highPoint.Y, 1.000001, "test for box Bounding box, highPoint.Y")
    Assert.Equal(boxBBox.highPoint.Z, 1.000001, "test for box Bounding box, highPoint.Z")


    //BBox Intersect test
    let bBox = BBox(Point(0.,0.,0.), Point(1.,1.,1.))
    let rayInside = Ray(Point(0.5,0.5,0.5), Vector(0.5,0.5,0.5))
    let hitInside = bBox.intersect(rayInside)
    Assert.True(hitInside.IsSome, "test on BBox intersect, for point inside BBox")
    let rayOutside = Ray(Point(1.5,1.5,1.5), Vector(-0.5,-0.5,-0.5))
    let hitOutside = bBox.intersect(rayOutside)
    Assert.True(hitOutside.IsSome, "test on BBox intersect, for point outside BBox")
    let rayOutsideMiss = Ray(Point(1.5,1.5,1.5), Vector(0.5,0.5,0.5))
    let hitOutsideMiss = bBox.intersect(rayOutsideMiss)
    Assert.True(hitOutsideMiss.IsNone, "test on BBox not intersect, for point outside BBox")


    //isInside function for shapes
    Assert.True(sphere.isInside (Point(0.,0.,0.)), "test isInside function, for point inside sphere")
    Assert.True(not(sphere.isInside (Point(7.,7.,7.))), "test isInside function, for point outside sphere")
    Assert.True(solidCylinder.isInside (Point(0.,0.,0.)), "test isInside function, for point inside solidCylinder")
    Assert.True(not(solidCylinder.isInside (Point(7.,7.,7.))), "test isInside function, for point outside solidCylinder")
    Assert.True(box.isInside (Point(0.5,0.5,0.5)), "test isInside function, for point inside box")
    Assert.True(not(box.isInside (Point(7.,7.,7.))), "test isInside function, for point outside box")


    //Tests that hitFunctions act as expected
    let rayHit =  Ray(Point(6., 3., 3.), Vector(-1., -0.5, -0.5))
    let rayMiss =  Ray(Point(6., 3., 3.), Vector(1., -0.5, -0.5))
    (*Assert.Equal((None,None,None), rectangle.hitFunction rayMiss, "hitFunction, rectangle, miss")
    Assert.Equal((None,None,None), disc.hitFunction rayMiss, "hitFunction, disc, miss")
    Assert.Equal((None,None,None), triangle.hitFunction rayMiss, "hitFunction, triangle, miss")
    Assert.Equal((None,None,None), sphere.hitFunction rayMiss, "hitFunction, sphere, miss")
    Assert.Equal((None,None,None), hollowCylinder.hitFunction rayMiss, "hitFunction, hollowCylinder, miss")
    Assert.Equal((None,None,None), box.hitFunction rayMiss, "hitFunction, box, miss")*)

    //Assert.Equal((Some(float),Some(Vector),Some(Material)), rectangle.hitFunction rayHit, "hitFunction, rectangle, hit")

    

    //Tests that toShape functions in BaseShape works
    let toRectangle = baseRectangle.toShape (Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.)))) :?> Rectangle
    let toDisc = baseDisc.toShape (Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.)))) :?> Disc
    let toTriangle = baseTriangle.toShape (Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.)))) :?> Triangle
    let toSphere = baseSphere.toShape (Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.)))) :?> SphereShape
    let toHollowCylinder = baseHollowCylinder.toShape (Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.)))) :?> HollowCylinder

    Assert.Equal(toRectangle.bottomleft, Point(0.,0.,0.), "rectangle.bottomLeft")
    Assert.Equal(rectangle.topLeft, Point(0.,1.,0.), "rectangle.topLeft")
    Assert.Equal(rectangle.bottomRight, Point(1.,0.,0.), "rectangle.bottomRight")

    Assert.Equal(toDisc.center, Point(0.,0.,0.), "disc.center")
    Assert.Equal(toDisc.radius, 2., "disc.radius")

    Assert.Equal(toTriangle.a, Point(0.,0.,0.), "triangle.a")
    Assert.Equal(toTriangle.b, Point(0.,1.,0.), "triangle.b")
    Assert.Equal(toTriangle.c, Point(1.,0.,0.), "triangle.c")

    Assert.Equal(toSphere.Origin, Point(0.,0.,0.), "sphere.origin")
    Assert.Equal(toSphere.Radius, 2., "sphere.radius")

    Assert.Equal(toHollowCylinder.center, Point(0.,0.,0.), "hollowCylinder.center")
    Assert.Equal(toHollowCylinder.radius, 2., "hollowCylinder.radius")
    Assert.Equal(toHollowCylinder.height, 4., "hollowCylinder.height")


    //test that all shapes hitfunctions act as expected
    //test that shapes are built as expected - Done
    //test that BaseShapes are built as expected - Done
    //test BaseShape to Shape functions - Done
    //test isInside function - Done
    //test getBoundingBox functions - Done

    
    ////CSG TESTS////

    //test isInside functions for CSG
    //test get BoundingBox for CSG
    //test hitFunctions for CSG

