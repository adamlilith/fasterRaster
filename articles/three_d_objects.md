# 3-dimensional objects

**fasterRaster** has limited support for 3-dimensional `GRaster`s and
`GVector`s. A `GRaster` is like a multi-layered raster, except that
layers contain values in “voxels” (3-dimensional pixels), where each
layer has a z-resolution, in addition to its normal x- and
y-resolutions. These layers are called “depths”. A 3-dimensional vector
has a z-dimension, so any point (i.e., an actual point, or points that
together define a line or polygon) has a third dimension. You can test
if an object is 3-dimensional using
[`topology()`](https://github.com/adamlilith/fasterRaster/reference/topology-GSpatial-method.md),
[`is.2d()`](https://github.com/adamlilith/fasterRaster/reference/is.2d.md),
or
[`is.3d()`](https://github.com/adamlilith/fasterRaster/reference/is.2d.md),
or by looking at its metadata (i.e., just enter the name of the object
in **R** and look at what is displayed).

Thus far, *no functions that handle 3-dimensional `GRaster`s or
`GVector`s have been tested.* Some functions like
[`spatSample()`](https://github.com/adamlilith/fasterRaster/reference/spatSample.md)
have arguments that allow you to do something with a z-component. Also,
some of the “getter” functions can report information about the third
dimension of a `GRaster` or `GVector`.

Three-dimensional `GRaster`s and `GVector`s should (?… remember, no
testing yet!) work with most functions. In some cases, the third
dimension will be ignored or removed. You should get a warning in these
cases.

## **fasterRaster** functions that specifically work with 3-dimensional objects

| **Function**                                                                                     | **Object type**        |
|--------------------------------------------------------------------------------------------------|------------------------|
| [`bottom()`](https://github.com/adamlilith/fasterRaster/reference/ext.md)                        | `GRaster` or `GVector` |
| [`dim3d()`](https://github.com/adamlilith/fasterRaster/reference/dim.md)                         | `GRaster`              |
| [`is.2d()`](https://github.com/adamlilith/fasterRaster/reference/is.2d.md)                       | `GRaster` or `GVector` |
| [`is.3d()`](https://github.com/adamlilith/fasterRaster/reference/is.2d.md)                       | `GRaster` or `GVector` |
| [`ncell3d()`](https://github.com/adamlilith/fasterRaster/reference/dim.md)                       | `GRaster`              |
| [`ndepth()`](https://github.com/adamlilith/fasterRaster/reference/dim.md)                        | `GRaster`              |
| [`res3d()`](https://github.com/adamlilith/fasterRaster/reference/res.md)                         | `GRaster`              |
| [`top()`](https://github.com/adamlilith/fasterRaster/reference/ext.md)                           | `GRaster` or `GVector` |
| [`topology()`](https://github.com/adamlilith/fasterRaster/reference/topology-GSpatial-method.md) | `GRaster` or `GVector` |
| [`zext()`](https://github.com/adamlilith/fasterRaster/reference/ext.md)                          | `GRaster` or `GVector` |
| [`zres()`](https://github.com/adamlilith/fasterRaster/reference/res.md)                          | `GRaster`              |

~ FINIS ~
