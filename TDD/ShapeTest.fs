module ShapeTest

open Tracer.Shapes
open Tracer.BaseShape
open Assert
open Tracer.Basics
open System


let allTest = 
    printfn "Shape Tests"
    let rectangle = new Rectangle(Point(0.,0.,0.), Point(0.,1.,0.), Point(1.,0.,0.), MatteMaterial(Colour(1.,1.,1.)))
    let disc = new Disc(Point(0.,0.,0.), 2., MatteMaterial(Colour(1.,1.,1.)))
    let triangle = new Triangle(Point(0.,0.,0.), Point(0.,1.,0.), Point(1.,0.,0.), MatteMaterial(Colour(1.,1.,1.)))
    let sphere = new SphereShape(Point(0.,0.,0.), 2., MatteMaterial(Colour(1.,1.,1.)))
    let hollowCylinder = new HollowCylinder(Point(0.,0.,0.), 2., 4., MatteMaterial(Colour(1.,1.,1.)))
    //let solidCylinder = NotImplementedException
    let box = new Box(Point(0.,0.,0.), Point(1.,1.,1.), MatteMaterial(Colour(1.,1.,1.)), MatteMaterial(Colour(1.,1.,1.)), 
                                                        MatteMaterial(Colour(1.,1.,1.)), MatteMaterial(Colour(1.,1.,1.)), 
                                                        MatteMaterial(Colour(1.,1.,1.)), MatteMaterial(Colour(1.,1.,1.)))
    let plane = new InfinitePlane(MatteMaterial(Colour(1.,1.,1.)))

    let baseRectangle = new BaseRectangle(Point(0.,0.,0.), Point(0.,1.,0.), Point(1.,0.,0.))
    let baseDisc = new BaseDisc(Point(0.,0.,0.), 2.)
    let baseTriangle = new BaseTriangle(Point(0.,0.,0.), Point(0.,1.,0.), Point(1.,0.,0.))
    let baseSphere = new BaseSphere(Point(0.,0.,0.), 2.)
    let baseHollowCylinder = new BaseHollowCylinder(Point(0.,0.,0.), 2., 4.)


    //Tests that shapes are built correctly
    Assert.Equal(rectangle.bottomLeft, Point(0.,0.,0.), "rectangle.bottomLeft")
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


    //Tests that hitFunctions act as expected






    

    //test that all shapes hitfunctions act as expected
    //test that shapes are built as expected
    //test that BaseShapes are built as expected
    //test BaseShape to Shape functions
    

