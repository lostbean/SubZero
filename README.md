# SubZero

**Subdivision Surfaces Library for Haskell**

SubZero is a Haskell library for creating and transforming smooth subdivision surfaces. It implements subdivision schemes for both 1D line segments and 2D triangular meshes, based on Catmull-Clark-style rules adapted for triangle meshes. The library supports full crease and corner vertex marking, limit surface evaluation, tangent and normal computation, and VTK rendering output.

## Features

- 1D line subdivision with fixed crease endpoints
- 2D surface subdivision for triangular meshes (Loop-style subdivision)
- Crease and corner vertex support for sharp feature preservation
- Limit surface position evaluation (the position vertices converge to under infinite subdivision)
- Tangent and normal vector computation at each vertex
- Decoupled mesh topology and geometry, allowing control point updates without rebuilding connectivity
- VTK output for visualization via the `hammer` library

## Modules

### `SubZero` (public API)

The top-level module that re-exports the public interface and defines two typeclasses:

- **`SubZero s`** -- Generic subdivision interface with:
  - `subdivide :: s -> s` -- Perform one level of subdivision.
  - `subdivideN :: Int -> s -> s` -- Perform N levels of subdivision.
- **`RenderSubZero s`** -- VTK rendering interface with:
  - `renderSub :: s -> VTK Vec3D` -- Convert a subdivided structure to a VTK object.

Instances are provided for `SubOne Vec2D`, `SubOne Vec3D`, `SubTwo Vec2D`, and `SubTwo Vec3D`.

### `SubZero.SubOne` (1D line subdivision)

Implements subdivision for open line segments. Open ends are treated as crease vertices and kept fixed during subdivision. The subdivision rule for interior vertices uses the cubic B-spline mask: `(1/8)(prev + 6*current + next)`, and new edge midpoints are simple averages `(1/2)(left + right)`.

**Key data types:**

- **`SubOne v`** -- Holds the mesh topology (`SubOneMesh`) and vertex positions (`Vector v`).
- **`SubOneMesh`** -- Topology-only structure containing:
  - `subOnePointers :: Vector Int` -- Indirect references into an external control point array.
  - `subOneLevel :: Level` -- Current subdivision level.
  - `subOneNSeg :: NSegm` -- Initial number of line segments.
- **`Level`** -- Newtype over `Int` representing the subdivision level.
- **`NSegm`** -- Newtype over `Int` representing the number of initial segments.

**Key functions:**

| Function           | Signature                                    | Description                                                                                          |
| ------------------ | -------------------------------------------- | ---------------------------------------------------------------------------------------------------- |
| `mkSubOne`         | `Vector Int -> Vector v -> Maybe (SubOne v)` | Construct a subdivision line from index and point arrays. Returns `Nothing` if fewer than 2 indices. |
| `mkSubOneMesh`     | `Vector Int -> Maybe SubOneMesh`             | Construct just the topology (no geometry).                                                           |
| `mkSubOneFromMesh` | `Vector v -> SubOneMesh -> SubOne v`         | Reconstruct a `SubOne` from points and an existing mesh topology.                                    |
| `subdivideOne`     | `SubOne v -> SubOne v`                       | One level of subdivision.                                                                            |
| `subdivideOneN`    | `Int -> SubOne v -> SubOne v`                | N levels of subdivision.                                                                             |
| `subOneLimit`      | `SubOne v -> Vector v`                       | Compute the limit positions (where vertices converge under infinite subdivision).                    |
| `subOneTan`        | `SubOne v -> Vector v`                       | Compute normalized tangent vectors at each vertex via finite differences.                            |
| `renderSubOne`     | `SubOne v -> VTK v`                          | Render to a VTK polyline.                                                                            |

### `SubZero.SubTwo` (2D surface subdivision)

Implements subdivision for triangular meshes. The algorithm and masks are based on:

- "Piecewise Smooth Surface Reconstruction"
- "High Performance Subdivision Surfaces"
- "Implementation of Triangle Subdivision for Holding Sharp Features with Flatness Control"

Each subdivision step splits every triangle into 4 sub-triangles by inserting a new vertex at each edge midpoint and connecting them. Vertex update rules depend on the vertex type (smooth, corner, boundary).

**Key data types:**

- **`SubTwo v`** -- Holds mesh connectivity (`MeshConn`) and vertex positions (`Vector v`).
- **`MeshConn`** -- The mesh connectivity structure containing:
  - `vertexType :: Vector VertexType` -- Whether each vertex is `SmoothVertex` or `CornerVertex`.
  - `vertexConn :: Vector VertexConn` -- For each vertex, the list of incident edges.
  - `edgeConn :: Vector EdgeConn` -- For each edge: the two adjacent faces and the two endpoint vertices.
  - `faceConn :: Vector FaceConn` -- For each face (triangle): the three vertex IDs.
  - `controlPointers :: Vector Int` -- Maps internal vertex indices to positions in an external control point array.
- **`VertexID`**, **`EdgeID`**, **`FaceID`** -- Newtype wrappers over `Int` for type-safe mesh element indexing.
- **`VertexType`** -- Either `SmoothVertex` or `CornerVertex`.
- **`TableAccess a`** -- Type class providing indexed read/write access to mutable and immutable vectors using the typed ID newtypes.

**Key functions:**

| Function           | Signature                                          | Description                                                                                   |
| ------------------ | -------------------------------------------------- | --------------------------------------------------------------------------------------------- |
| `mkSubTwo`         | `Vector v -> [(Int,Int,Int)] -> [Int] -> SubTwo v` | Construct a subdivision surface from a point array, triangle list, and corner vertex indices. |
| `mkSubTwoFromMesh` | `Vector v -> MeshConn -> SubTwo v`                 | Reconstruct from points and an existing mesh topology.                                        |
| `buildMesh`        | `[(Int,Int,Int)] -> [Int] -> MeshConn`             | Build the mesh connectivity from triangles and corner indices.                                |
| `subdivideTwo`     | `SubTwo v -> SubTwo v`                             | One level of subdivision. Splits each triangle into 4.                                        |
| `subdivideTwoN`    | `Int -> SubTwo v -> SubTwo v`                      | N levels of subdivision.                                                                      |
| `subTwoLimit`      | `SubTwo v -> SubTwo v`                             | Compute limit surface positions for all vertices.                                             |
| `subTwoNormals`    | `SubTwo Vec3D -> Vector Vec3D`                     | Compute vertex normals (cross product of tangent vectors).                                    |
| `subTwoTans`       | `SubTwo Vec3D -> Vector (Vec3D, Vec3D)`            | Compute two tangent vectors at each vertex.                                                   |
| `renderSubTwo`     | `SubTwo Vec3D -> VTK Vec3D`                        | Render to a VTK triangulation.                                                                |

**Subdivision vertex rules:**

- **Smooth interior vertex:** Updated using the weight `w = (40 - (3 + 2*cos(2*pi/n))^2) / 64` where `n` is the vertex valence. The new position is `(1 - w) * v + (w / n) * sum(neighbors)`.
- **Boundary vertex (2 boundary edges):** Updated as `(1/8)(neighbor1 + 6*v + neighbor2)`.
- **Corner vertex:** Position is unchanged.
- **New edge vertex (interior edge):** `(1/8)(v1 + v2 + sum_of_face_vertices_of_both_adjacent_faces)`.
- **New edge vertex (boundary edge):** Simple midpoint `(1/2)(v1 + v2)`.

### `SubZero.Internal` (advanced internals)

Re-exports internal types for advanced use cases:

- `Level`, `NSegm`, `getSubOneArrSize` (from `SubOne`)
- `TableAccess`, `VertexID`, `EdgeID`, `FaceID` (from `SubTwo`)

## Usage Examples

### 1D Line Subdivision

```haskell
import qualified Data.Vector as V
import Linear.Vect
import SubZero

-- Define control points
vertices :: V.Vector Vec3D
vertices = V.fromList
    [ Vec3 0 0 0.5
    , Vec3 0 1 0
    , Vec3 1 2 0
    ]

-- Create a subdivision line from indices [0, 1, 2] into the vertex array
case mkSubOne (V.fromList [0, 1, 2]) vertices of
    Just line -> do
        let refined = subdivideN 3 line     -- 3 levels of subdivision
        let tangents = subOneTan refined    -- tangent at each vertex
        let limits = subOneLimit refined    -- limit positions
        let vtk = renderSub refined         -- VTK output
        pure ()
    Nothing -> putStrLn "Need at least 2 indices"
```

### 2D Surface Subdivision

```haskell
import qualified Data.Vector as V
import Linear.Vect
import SubZero

-- Define control points
vertices :: V.Vector Vec3D
vertices = V.fromList
    [ Vec3 0 0 0,   Vec3 1 0 0,   Vec3 0.5 1 0
    , Vec3 0 0 1,   Vec3 1 0 1,   Vec3 0.5 1 1
    ]

-- Define triangular faces (vertex index triples) and corner vertices
let triangles = [(0, 1, 2), (3, 4, 5), (0, 1, 4), (0, 4, 3)]
let corners   = [2, 5]  -- vertices 2 and 5 are sharp corners

-- Build the subdivision surface
let surface = mkSubTwo vertices triangles corners

-- Subdivide 3 times
let refined = subdivideN 3 surface

-- Compute geometric properties
let limitSurface = subTwoLimit refined
let normals      = subTwoNormals refined
let tangentPairs = subTwoTans refined

-- Export to VTK for visualization
let vtk = renderSub refined
```

### Separating Topology from Geometry

A key design feature is that mesh topology (`SubOneMesh` / `MeshConn`) is decoupled from geometry. You can rebuild a subdivision object with new control point positions without recomputing connectivity:

```haskell
-- Build mesh topology once
let mesh = buildMesh [(0,1,2), (1,2,3)] [0, 3]

-- Use with different point sets
let surface1 = mkSubTwoFromMesh pointsA mesh
let surface2 = mkSubTwoFromMesh pointsB mesh
```

This is useful in animation or optimization workflows where vertex positions change but connectivity remains fixed.

## Dependencies

| Package       | Version   | Purpose                                                                    |
| ------------- | --------- | -------------------------------------------------------------------------- |
| `base`        | >= 4, < 5 | Standard library                                                           |
| `containers`  | >= 0.5    | `IntMap`, `IntSet`, `Map` for mesh construction                            |
| `hammer`      | >= 0.3    | VTK rendering (`Hammer.VTK`) and segment sorting (`Hammer.Math.SortSeq`)   |
| `linear-vect` | >= 0.2    | Linear algebra types (`Vec2D`, `Vec3D`) and operations                     |
| `primitive`   | >= 0.5    | Low-level mutable operations for in-place mesh mutation during subdivision |
| `vector`      | >= 0.10   | Core data structure for all arrays                                         |

Both `hammer` and `linear-vect` are sibling packages in the same repository.

## Building

### With Nix (recommended)

```bash
nix develop
cabal build
```

### With Cabal

```bash
cabal build --allow-newer
```

### With Stack

```bash
stack build
```

### Testing & Coverage

The project includes a comprehensive test suite using `tasty`, `HUnit`, and `QuickCheck`.

#### Running Tests

To run the automated tests:

```bash
cabal test SubZero-tests
```

#### Tracking Coverage with HPC

To generate a test coverage report using **HPC (Haskell Program Coverage)**:

1. Run the tests with coverage enabled:
   ```bash
   cabal test SubZero-tests --enable-coverage
   ```
2. The coverage report will be generated in HTML format. Look for the output path in the terminal, typically:
   `dist-newstyle/build/.../SubZero-0.1.1/t/SubZero-tests/hpc/vanilla/html/hpc_index.html`

### Test / Profile Executable

The package includes a `SubZero-test` executable (guarded behind the `test` flag) that creates sample 1D and 2D subdivisions and writes VTK files for visualization:

```bash
cabal build -f test
cabal run SubZero-test
```

The output `.vtu` files can be visualized with [ParaView](https://www.paraview.org/) or any VTK-compatible viewer.

## Architecture Notes

The subdivision algorithm operates in two distinct phases per level:

1. **Topology subdivision** (`subdivideTwoConn`): Splits edges and faces using mutable vectors in the `ST` monad for performance. Each triangle becomes 4 triangles; each edge is split into 2; and 3 new internal edges are created per face. This runs entirely on connectivity data without touching vertex positions.

2. **Geometry subdivision** (`subdivideTwoPoints`): Computes new vertex positions using the subdivision masks. Existing vertices are updated in place according to their type (smooth/corner/boundary), and new edge-midpoint vertices are computed from neighboring vertex and face positions.

This separation means topology subdivision is done once, while geometry can be recomputed cheaply when only vertex positions change.

## Author

Edgar Gomes de Araujo (<talktoedgar@gmail.com>)

## License

MIT -- see [LICENSE](./LICENSE).
