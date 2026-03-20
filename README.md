# SubZero

Subdivision surfaces for smooth mesh refinement.

## What is this?

A Haskell library for creating smooth surfaces from coarse polygon meshes using subdivision. Starting from a rough triangular mesh (or a polyline), each subdivision step inserts new vertices and repositions existing ones according to weighted averaging rules, progressively approaching a smooth limit surface. After a few levels of subdivision, the originally faceted mesh becomes visually smooth.

The library implements two subdivision schemes:

- **1D line subdivision** -- refines open line segments into smooth curves
- **2D surface subdivision** -- refines triangular meshes into smooth surfaces using the Loop subdivision scheme

## Background: what is subdivision?

Subdivision is a technique from computer graphics and geometric modeling. The idea: start with a coarse "control mesh" that roughly defines the shape you want, then repeatedly refine it. Each refinement step:

1. Splits each element (edge or triangle) into smaller elements
2. Computes new vertex positions as weighted averages of nearby vertices

The weights are chosen so that after infinite subdivision, the surface converges to a well-defined smooth limit surface (typically C2-continuous at regular vertices). In practice, 3--4 levels of subdivision are usually enough for a visually smooth result.

## 1D line subdivision

Refines an open polyline into a smooth curve. Endpoints are treated as crease vertices and stay fixed. Interior vertices are repositioned using the cubic B-spline mask:

- Existing vertex: weighted average of itself and its two neighbors -- `(1/8)(prev + 6*current + next)`
- New edge midpoint: average of its two endpoints -- `(1/2)(left + right)`

This produces curves that converge to uniform cubic B-splines.

## 2D surface subdivision (Loop scheme)

Refines a triangular mesh: each triangle is split into 4 sub-triangles by inserting new vertices at edge midpoints. Vertex positions are updated using masks that depend on the vertex type:

- **Smooth interior vertices** -- repositioned using a weight that depends on the vertex valence (number of neighboring vertices). The formula ensures C2 continuity at regular (valence-6) vertices.
- **Boundary vertices** -- updated with the same B-spline mask as the 1D case: `(1/8)(neighbor1 + 6*vertex + neighbor2)`
- **Corner vertices** -- remain fixed, creating sharp features in the surface
- **New edge vertices** -- positioned based on the four vertices of the two triangles sharing that edge (interior) or as simple midpoints (boundary)

### Limit surface evaluation

Rather than subdividing many times, the library can compute the exact positions where vertices would converge under infinite subdivision. This gives the mathematically precise smooth surface without the cost of many subdivision levels.

### Sharp features

Not all edges and vertices need to be smooth. Marking vertices as "corners" keeps them fixed during subdivision, creating sharp points. Boundary edges are automatically detected and handled with B-spline rules, creating smooth creases along mesh boundaries.

### Topology/geometry separation

A key design feature: the mesh connectivity (which vertices connect to which) is separated from the vertex positions. The topology subdivision (splitting triangles, updating adjacency tables) is done once. After that, you can cheaply recompute vertex positions with different control points without rebuilding the topology -- useful for animation or optimization workflows.

## Example

```haskell
import qualified Data.Vector as V
import Linear.Vect
import SubZero

-- Define control points and triangular faces
let vertices  = V.fromList [Vec3 0 0 0, Vec3 1 0 0, Vec3 0.5 1 0, Vec3 0.5 0.5 1]
    triangles = [(0,1,2), (0,1,3), (1,2,3), (0,2,3)]
    corners   = []  -- no sharp corners

    surface = mkSubTwo vertices triangles corners
    refined = subdivideN 3 surface       -- 3 levels of Loop subdivision
    normals = subTwoNormals refined      -- vertex normals
    vtk     = renderSub refined          -- VTK output for ParaView
```

## Where is it used?

- **VirMat** -- the virtual microstructure generator uses SubZero to smooth the Voronoi grain boundaries. After constructing the Voronoi tessellation (which produces flat-faceted polyhedra), Loop subdivision is applied to produce smooth, realistic grain shapes suitable for visualization and further analysis.

## How to build

```bash
# With Nix (recommended)
nix develop
cabal build --allow-newer

# With Cabal
cabal build

# Run tests
cabal test SubZero-tests
```

## License

MIT -- see [LICENSE](./LICENSE).
