module ShapeTest

open Tracer.Shapes
open Assert
open Tracer.Basics

let allTest = 
    printfn "Shape Tests"
    let rectangle = new Rectangle(Point(0.,0.,0.), Point(0.,1.,0.), Point(1.,0.,0.), Material())
    let disc = new Disc(Point(0.,0.,0.), 2., Material())
    let triangle = new Triangle(Point(0.,0.,0.), Point(0.,1.,0.), Point(1.,0.,0.), Material())
    let sphere = new Sphere(Point(0.,0.,0.), 2., Material())
    let hollowCylinder = new HollowCylinder(Point(0.,0.,0.), 2., 4., Material())
    let solidCylinder = new SolidCylinder() //not implemented yet
    //test that all shapes hitfunctions act as expected
    //test that shapes are built as expected
    //test that BaseShapes are built as expected
    

