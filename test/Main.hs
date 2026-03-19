{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Data.Vector as V
import Linear.Vect
import SubZero
import SubZero.SubOne

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [subOneTests, subTwoTests, classTests]

-- Generator for a list of at least 2 points
genPoints :: Gen [Vec3D]
genPoints = do
    n <- choose (2, 20)
    vectorOf n (Vec3 <$> arbitrary <*> arbitrary <*> arbitrary)

-- Helper to check if two vectors of Vec3D are close
isClose :: Vec3D -> Vec3D -> Bool
isClose (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
    abs (x1 - x2) < 1e-9 && abs (y1 - y2) < 1e-9 && abs (z1 - z2) < 1e-9

subOneTests :: TestTree
subOneTests =
    testGroup
        "SubOne Tests"
        [ testGroup
            "HUnit"
            [ testCase "subdivideN 0 is identity" $
                let points = V.fromList [Vec3 0 0 0, Vec3 1 1 1, Vec3 2 2 2] :: V.Vector Vec3D
                    indices = V.fromList [0, 1, 2]
                 in case mkSubOne indices points of
                        Just sub ->
                            let sub' = subdivideN 0 sub
                             in V.toList (subOnePoints sub') @?= V.toList (subOnePoints sub)
                        Nothing -> assertFailure "mkSubOne failed"
            , testCase "subOneTan" $
                let points = V.fromList [Vec3 0 0 0, Vec3 1 0 0, Vec3 2 0 0] :: V.Vector Vec3D
                    indices = V.fromList [0, 1, 2]
                 in case mkSubOne indices points of
                        Just sub ->
                            let tans = subOneTan sub
                             in V.length tans @?= 3
                        Nothing -> assertFailure "mkSubOne failed"
            ]
        , testGroup
            "QuickCheck"
            [ testProperty "subdivideN n . subdivideN m == subdivideN (n+m)" $
                forAll genPoints $ \ptsList ->
                    forAll (choose (0, 3)) $ \n ->
                        forAll (choose (0, 3)) $ \m ->
                            let points = V.fromList ptsList
                                indices = V.fromList [0 .. V.length points - 1]
                             in case mkSubOne indices points of
                                    Just sub ->
                                        let subNM = subdivideN (n + m) sub
                                            subN_M = subdivideN n (subdivideN m sub)
                                         in V.toList (subOnePoints subNM) == V.toList (subOnePoints subN_M)
                                    Nothing -> False
            , testProperty "point count follows getSubOneArrSize" $
                forAll genPoints $ \ptsList ->
                    forAll (choose (0, 5)) $ \n ->
                        let points = V.fromList ptsList
                            indices = V.fromList [0 .. V.length points - 1]
                         in case mkSubOne indices points of
                                Just sub ->
                                    let subN = subdivideN n sub
                                        expectedSize = getSubOneArrSize (subOneNSeg (subOneMesh sub)) (fromIntegral n)
                                     in V.length (subOnePoints subN) == expectedSize
                                Nothing -> False
            , testProperty "endpoints are fixed" $
                forAll genPoints $ \ptsList ->
                    forAll (choose (0, 5)) $ \n ->
                        let points = V.fromList ptsList
                            indices = V.fromList [0 .. V.length points - 1]
                         in case mkSubOne indices points of
                                Just sub ->
                                    let subN = subdivideN n sub
                                        p0 = V.head (subOnePoints sub)
                                        pn = V.last (subOnePoints sub)
                                     in V.head (subOnePoints subN) == p0 && V.last (subOnePoints subN) == pn
                                Nothing -> False
            , testProperty "subdivided points at even indices are previous limit positions" $
                forAll genPoints $ \ptsList ->
                    let points = V.fromList ptsList
                        indices = V.fromList [0 .. V.length points - 1]
                     in case mkSubOne indices points of
                            Just sub ->
                                let sub1 = subdivide sub
                                    lim0 = subOneLimit sub
                                    pts1 = subOnePoints sub1
                                    -- Every second point in pts1 should correspond to a point in lim0
                                    pts1_subset = V.generate (V.length lim0) (\i -> pts1 V.! (i * 2))
                                 in V.all id (V.zipWith isClose lim0 pts1_subset)
                            Nothing -> False
            ]
        ]

eulerChar :: MeshConn -> Int
eulerChar mesh =
    V.length (vertexConn mesh) - V.length (edgeConn mesh) + V.length (faceConn mesh)

subTwoTests :: TestTree
subTwoTests =
    testGroup
        "SubTwo Tests"
        [ testCase "Minimal triangle mesh" $
            let points = V.fromList [Vec3 0 0 0, Vec3 1 0 0, Vec3 0 1 0] :: V.Vector Vec3D
                faces = [(0, 1, 2)]
                sub = mkSubTwo points faces []
                sub1 = subdivide sub
             in do
                    V.length (faceConn (subTwoMesh sub1)) @?= 4
                    V.length (subTwoPoints sub1) @?= 6
                    eulerChar (subTwoMesh sub) @?= eulerChar (subTwoMesh sub1)
        , testCase "Tetrahedron" $
            let points = V.fromList [Vec3 1 1 1, Vec3 1 (-1) (-1), Vec3 (-1) 1 (-1), Vec3 (-1) (-1) 1] :: V.Vector Vec3D
                faces = [(0, 1, 2), (0, 2, 3), (0, 3, 1), (1, 3, 2)]
                sub = mkSubTwo points faces []
                sub1 = subdivide sub
                sub2 = subdivide sub1
             in do
                    eulerChar (subTwoMesh sub) @?= 2
                    eulerChar (subTwoMesh sub1) @?= 2
                    eulerChar (subTwoMesh sub2) @?= 2
                    V.length (faceConn (subTwoMesh sub1)) @?= 16
                    V.length (faceConn (subTwoMesh sub2)) @?= 64
        , testCase "Tetrahedron with corners" $
            let points = V.fromList [Vec3 1 1 1, Vec3 1 (-1) (-1), Vec3 (-1) 1 (-1), Vec3 (-1) (-1) 1] :: V.Vector Vec3D
                faces = [(0, 1, 2), (0, 2, 3), (0, 3, 1), (1, 3, 2)]
                corners = [0]
                sub = mkSubTwo points faces corners
                sub1 = subdivide sub
             in do
                    -- Corner point should remain exactly the same
                    (subTwoPoints sub1 V.! 0) @?= (subTwoPoints sub V.! 0)
                    eulerChar (subTwoMesh sub1) @?= 2
        , testCase "subTwoLimit and subTwoTans" $
            let points = V.fromList [Vec3 1 1 1, Vec3 1 (-1) (-1), Vec3 (-1) 1 (-1), Vec3 (-1) (-1) 1] :: V.Vector Vec3D
                faces = [(0, 1, 2), (0, 2, 3), (0, 3, 1), (1, 3, 2)]
                sub = mkSubTwo points faces []
                subL = subTwoLimit sub
                tans = subTwoTans sub
                norms = subTwoNormals sub
             in do
                    V.length (subTwoPoints subL) @?= 4
                    V.length tans @?= 4
                    V.length norms @?= 4
        , testCase "Mesh with open boundary" $
            -- Two triangles forming a square, has 4 boundary edges
            let points = V.fromList [Vec3 0 0 0, Vec3 1 0 0, Vec3 1 1 0, Vec3 0 1 0] :: V.Vector Vec3D
                faces = [(0, 1, 2), (0, 2, 3)]
                sub = mkSubTwo points faces []
                sub1 = subdivide sub
             in do
                    V.length (subTwoPoints sub1) @?= 9
                    eulerChar (subTwoMesh sub1) @?= 1 -- Square is disk-like, chi=1
        ]

classTests :: TestTree
classTests =
    testGroup
        "Typeclass Tests"
        [ testCase "SubZero instance for SubOne" $
            let points = V.fromList [Vec3 0 0 0, Vec3 1 0 0] :: V.Vector Vec3D
                indices = V.fromList [0, 1]
             in case mkSubOne indices points of
                    Just (sub :: SubOne Vec3D) ->
                        let sub1 = subdivide sub
                            sub2 = subdivideN 2 sub
                         in do
                                V.length (subOnePoints sub1) @?= 3
                                V.length (subOnePoints sub2) @?= 5
                    Nothing -> assertFailure "mkSubOne failed"
        , testCase "SubZero instance for SubTwo" $
            let points = V.fromList [Vec3 0 0 0, Vec3 1 0 0, Vec3 0 1 0] :: V.Vector Vec3D
                faces = [(0, 1, 2)]
                sub = mkSubTwo points faces []
                sub1 = subdivide sub
                sub2 = subdivideN 2 sub
             in do
                    V.length (subTwoPoints sub1) @?= 6
                    V.length (subTwoPoints sub2) @?= 15 -- (6 vertices + 9 edges = 15)
        , testCase "RenderSubZero instance for SubOne" $
            let points = V.fromList [Vec3 0 0 0, Vec3 1 0 0] :: V.Vector Vec3D
                indices = V.fromList [0, 1]
             in case mkSubOne indices points of
                    Just (sub :: SubOne Vec3D) ->
                        let _vtk = renderSub sub in pure ()
                    Nothing -> assertFailure "mkSubOne failed"
        , testCase "RenderSubZero instance for SubTwo" $
            let points = V.fromList [Vec3 0 0 0, Vec3 1 0 0, Vec3 0 1 0] :: V.Vector Vec3D
                faces = [(0, 1, 2)]
                sub = mkSubTwo points faces []
                _vtk = renderSub sub
             in pure ()
        ]
