using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace Advent.Text.ImageProcessing
{
    public class JigsawCamera
    {
        private readonly Dictionary<long, Tile> _tiles;
        /// <summary>
        /// For a given side, this holds the IDs of all tiles that contain that
        /// side after a rotation or flip
        /// </summary>
        private readonly Dictionary<string, List<long>> _allSides;

        /// <summary>
        /// The number of tiles along the length of the image
        /// </summary>
        public int ImageSizeInTiles { get; }

        private static readonly bool[] _boolVals = new bool[] { false, true };

        /// <summary>
        /// Create a new image from a string
        /// </summary>
        /// <param name="s">The stringof data</param>
        public JigsawCamera(string s)
        {
            _tiles = s.Trim().Split("\n\n")
                .Select(s => new Tile(s))
                .ToDictionary(tile => tile.Id);

            var numTiles = _tiles.Count;
            ImageSizeInTiles = (int)Math.Sqrt(numTiles);
            if (ImageSizeInTiles * ImageSizeInTiles != numTiles)
            {
                throw new ArgumentException("Number of tiles is not a square or there was a rounding error");
            }

            _allSides = _tiles.Values.SelectMany(tile => tile.GetAllSides().Select(side => (tile.Id, side)))
                .GroupBy(pair => pair.side)
                .ToDictionary(group => group.Key, group => group.Select(x => x.Id).ToList());
        }

        /// <summary>
        /// Find a way to fit the jigsaw pieces together
        /// </summary>
        /// <returns>A 2D array of triplets, the first element indicating the tile ID,
        /// the second the number of turns to the right that need to be made to the tile
        /// to get it to fit, and the third is whether the tile should be flipped over the
        /// vertical axis before rotating.</returns>
        public (long tileId, int numTurns, bool isFlipped)[,] Solve()
        {
            var trials = new Stack<( (long tileId, int numTurns, bool flipped)?[,] trial, (int row, int col) lastAdded, HashSet<long> used)>();

            // we'll use a depth first search to look for a solution. Start by
            // trying every piece in the upper left corner
            foreach (var tileId in _tiles.Keys)
            {
                for (var numTurns = 0; numTurns < 4; numTurns++)
                {
                    foreach (var flipped in _boolVals)
                    {
                        var trial = new (long tileId, int numTurns, bool flipped)?[ImageSizeInTiles, ImageSizeInTiles];
                        trial[0, 0] = (tileId, numTurns, flipped);
                        trials.Push((trial, (0, 0), new HashSet<long> { tileId }));
                    }
                }
            }

            while (trials.TryPop(out var data))
            {
                var (trial, lastAdded, used) = data;

                // we're going to take the work so far and try to add on the
                // next tile, going left to right then top to bottom
                var toAdd = (lastAdded.row, col: lastAdded.col + 1);
                if (toAdd.col >= ImageSizeInTiles)
                {
                    toAdd = (row: lastAdded.row + 1, col: 0);
                    // if we're trying to add below the picture, then that
                    // means that we're done. Let's map the input
                    if (toAdd.row >= ImageSizeInTiles)
                    {
                        var result = new (long tileId, int numTurns, bool isFlipped)[ImageSizeInTiles, ImageSizeInTiles];
                        for (var i = 0; i < ImageSizeInTiles; i++)
                        {
                            for (var j = 0; j < ImageSizeInTiles; j++)
                            {
                                result[i, j] = trial[i, j].Value;
                            }
                        }
                        return result;
                    }
                }

                // only consider tiles we haven't used yet
                var possibleMatches = _tiles.Keys.Where(tileId => !used.Contains(tileId));

                // only consider tiles that can fit the one above
                var topSide = (string)null;
                if (toAdd.row > 0)
                {
                    var (topSideId, topSideNumTurns, topSideFlipped) = trial[toAdd.row - 1, toAdd.col].Value;
                    topSide = string.Concat(_tiles[topSideId].GetSide(Side.Bottom, topSideNumTurns, topSideFlipped).Reverse());
                    possibleMatches = possibleMatches.Where(id => _allSides[topSide].Contains(id));
                }

                // only consider tiles that can fit the one below
                var leftSide = (string)null;
                if (toAdd.col > 0)
                {
                    var (leftSideId, leftSideNumTurns, leftSideFlipped) = trial[toAdd.row, toAdd.col - 1].Value;
                    leftSide = string.Concat(_tiles[leftSideId].GetSide(Side.Right, leftSideNumTurns, leftSideFlipped).Reverse());
                    possibleMatches = possibleMatches.Where(id => _allSides[leftSide].Contains(id));
                }

                // take the tiles that match, and try to find the right
                // orientation to fit it in
                foreach (var possibleMatch in possibleMatches)
                {
                    var matchTile = _tiles[possibleMatch];
                    for (var numTurns = 0; numTurns < 4; numTurns++)
                    {
                        foreach (var flipped in _boolVals)
                        {
                            if (topSide != null && matchTile.GetSide(Side.Top, numTurns, flipped) != topSide)
                            {
                                continue;
                            }

                            if (leftSide != null && matchTile.GetSide(Side.Left, numTurns, flipped) != leftSide)
                            {
                                continue;
                            }

                            var newTrial = trial.Clone() as (long tileId, int numTurns, bool flipped)?[,];
                            newTrial[toAdd.row, toAdd.col] = (matchTile.Id, numTurns, flipped);

                            var newUsed = new HashSet<long>(used) { matchTile.Id };
                            trials.Push((newTrial, (toAdd.row, toAdd.col), newUsed));
                        }
                    }
                }

            }

            return null;
        }

        /// <summary>
        /// Get the roughness of the image given the solution
        /// </summary>
        /// <param name="solution">The solution</param>
        /// <param name="patternString">The pattern of sea monster to look for</param>
        /// <returns>The roughness</returns>
        public int? GetRoughness(
            (long tileId, int numTurns, bool isFlipped)[,] solution,
            string patternString)
        {
            var pixels = GetPixelsForSolution(solution);

            //var pattern = new List<(int, int)>();

            // transform the raw string into a mask that we'll use
            var pattern = patternString.Split('\n')
                .Select(s => s.Select((c, i) => (c, i)).Where(pair => pair.c == '#').Select(pair => pair.i).ToList())
                .SelectMany((cols, row) => cols.Select(col => (row, col)))
                .ToList();

            //var pattern = patternString.Split('\n')
            //    .SelectMany((s, i) => (i, s.Select((c, j) => (c, j)).Where(pair => pair.c == '#').Select(pair => pair.j)))
            //    .ToList();

            return GetOrientations(pixels)
                .Select(p => GetRoughness(p, pattern))
                .FirstOrDefault(roughess => roughess.HasValue);
        }

        /// <summary>
        /// For a given solution, which consits of an array of tiles and their
        /// orientations, returns the corresponding solution in pixels.
        /// </summary>
        /// <param name="solution">The solution</param>
        /// <returns>The image data as an array of pixel values</returns>
        private char[,] GetPixelsForSolution((long tileId, int numTurns, bool isFlipped)[,] solution)
        {
            // the image size in pixels
            var tileSizeInPixels = Tile.TILE_SIZE - 2;
            var imageSize = tileSizeInPixels * ImageSizeInTiles;

            var pixels = new char[imageSize, imageSize];

            // patch the rotated and flipped tiles to form the over all image
            for (var i = 0; i < ImageSizeInTiles; i++)
            {
                var tilePixels = Enumerable.Range(0, ImageSizeInTiles)
                    .Select(j =>
                    {
                        var (tileId, numTurns, isFlipped) = solution[i, j];
                        var tile = _tiles[tileId];
                        return tile.GetPixels(numTurns, isFlipped);
                    })
                    .ToList();
                var numRows = tilePixels[0].Count;

                var rows = Enumerable.Range(0, numRows)
                    .Select(j => string.Concat(tilePixels.Select(x => x[j])))
                    .ToList();

                for (var k = 0; k < numRows; k++)
                {
                    for (var j = 0; j < imageSize; j++)
                    {
                        pixels[i * numRows + k, j] = rows[k][j];
                    }
                }
            }

            return pixels;
        }

        /// <summary>
        /// Get all possible rotations and flips of the square matrix of
        /// pixels
        /// </summary>
        /// <param name="pixels">A square matrix of pixels</param>
        /// <returns>All possible orientations</returns>
        private static IEnumerable<char[,]> GetOrientations(char[,] pixels)
        {
            yield return pixels;

            for (var i = 0; i < 3; i++)
            {
                pixels = RotateSquareMatrix(pixels);
                yield return pixels;
            }

            pixels = FlipSquareMatrix(pixels);
            yield return pixels;

            for (var i = 0; i < 3; i++)
            {
                pixels = RotateSquareMatrix(pixels);
                yield return pixels;
            }

        }

        /// <summary>
        /// Get the roughness for a particular set of pixels with a fixed
        /// orientation
        /// </summary>
        /// <param name="pixels">The pixels to scan</param>
        /// <param name="pattern">The pattern to search for</param>
        /// <returns>The roughness if found, null otherwise</returns>
        private static int? GetRoughness(char[,] pixels, List<(int row, int col)> pattern)
        {
            var inPattern = new HashSet<(int, int)>();
            var patternDeltaRow = pattern.Select(pair => pair.row).Max();
            var patternDeltaCol = pattern.Select(pair => pair.col).Max();
            var size = pixels.GetLength(0);

            for (var i = 0; i < size - patternDeltaRow; i++)
            {
                for (var j = 0; j < size - patternDeltaCol; j++)
                {
                    var toCheck = pattern.Select(pair => (row: i + pair.row, col: j + pair.col));
                    if (toCheck.All(pair => pixels[pair.row, pair.col] == '#'))
                    {
                        inPattern.UnionWith(toCheck);
                    }
                }
            }

            if (!inPattern.Any())
            {
                return null;
            }

            var result = Enumerable.Range(0, size)
                .Select(i => Enumerable.Range(0, size)
                    .Count(j => !inPattern.Contains((i, j)) && pixels[i, j] == '#'))
                .Sum();
            
            return result;
        }

        /// <summary>
        /// Get a flip of a square matrix
        /// </summary>
        /// <param name="m">The matrix to flip</param>
        /// <returns>The flipped matrix</returns>
        private static char[,] FlipSquareMatrix(char[,] m)
        {
            var newM = m.Clone() as char[,];

            var size = m.GetLength(0);

            for (var i = 0; i < size; i++)
            {
                for (var j = 0; j < size; j++)
                {
                    newM[i, j] = m[i, size - j - 1];
                }
            }
            return newM;
        }

        /// <summary>
        /// Rotate a square matrix
        /// </summary>
        /// <param name="m">The matrix to rotate</param>
        /// <returns>The rotated matrix</returns>
        private static char[,] RotateSquareMatrix(char[,] m)
        {
            var size = m.GetLength(0);
            if (size != m.GetLength(1))
            {
                throw new ArgumentException("Squares only");
            }

            if (size % 2 != 0)
            {
                throw new ArgumentException("Expected dimension with even length");
            }

            // rotate each ring, going outside in
            var newM = m.Clone() as char[,];
            for (var i = 0; i < size / 2; i++)
            {
                var sideSize = size - 2 * i;
                for (var j = 0; j < sideSize; j++)
                {
                    newM[i, i + j] = m[size - i - 1 - j, i];
                    newM[i + j, size - i - 1] = m[i, i + j];
                    newM[size - i - 1, size - i - 1 - j] = m[i + j, size - i - 1];
                    newM[size - i - 1 - j, i] = m[size - i - 1, size - i - 1 - j];
                }
            }

            return newM;
        }

        /// <summary>
        /// Represents one of the picture tiles
        /// </summary>
        private class Tile
        {
            public const int TILE_SIZE = 10;

            public long Id { get; }

            // these fields contain the data with the input orientations
            private readonly string[] _inputSides;
            private readonly string[] _inputSidesFlipped; // flipped about the vertical axis from the original

            private readonly string[] _inputData;

            private static readonly Regex _idRegex = new Regex(@"^Tile (?<tileId>\d+):$");
            public Tile(string s)
            {
                var lines = s.Trim().Split('\n');
                _inputData = lines.Skip(1).ToArray();

                var idMatches = _idRegex.Match(lines[0]);

                Id = long.Parse(idMatches.Groups["tileId"].Value);

                _inputSides = new string[4] {
                    lines[1],
                    string.Concat(lines[1..^0].Select(s => s.Last())),
                    string.Concat(lines[^1].Reverse()),
                    string.Concat(lines[1..^0].Reverse().Select(s => s[0]))
                };

                _inputSidesFlipped = new string[4]
                {
                    string.Concat(lines[1].Reverse()),
                    string.Concat(lines[1..^0].Select(s => s[0])),
                    lines[^1],
                    string.Concat(lines[1..^0].Reverse().Select(s => s.Last()))
                };
            }

            /// <summary>
            /// Get a side for this tile
            /// </summary>
            /// <param name="side">The side to get</param>
            /// <param name="numTurns">The number of turns to make before
            /// getting this side</param>
            /// <param name="flipped">Whether the tile should be flipped about
            /// the vertical axis first</param>
            /// <returns>The side</returns>
            public string GetSide(Side side, int numTurns, bool flipped)
            {
                var sides = flipped ? _inputSidesFlipped : _inputSides;
                return sides[((int)side - numTurns + 4) % 4];
            }

            /// <summary>
            /// Get all possible sides, considering all flips
            /// </summary>
            /// <returns>All sides</returns>
            public IEnumerable<string> GetAllSides()
                => _inputSides.Concat(_inputSidesFlipped);

            /// <summary>
            /// Get the data for this tile, after turning and flipping it.
            /// </summary>
            /// <param name="numTurns">The number of turns</param>
            /// <param name="flipped">Whether the tile is flipped</param>
            /// <returns></returns>
            public List<string> GetPixels(int numTurns, bool flipped)
            {
                var data = new char[TILE_SIZE - 2, TILE_SIZE - 2];
                for (var i = 0; i < TILE_SIZE - 2; i++)
                {
                    var row = _inputData[i + 1].ToCharArray();
                    if (flipped)
                    {
                        Array.Reverse(row);
                    }
                    for (var j = 0; j < TILE_SIZE - 2; j++)
                    {
                        data[i, j] = row[j + 1];
                    }
                }

                for (var i = 0; i < numTurns; i++)
                {
                    data = RotateSquareMatrix(data);
                }

                return Enumerable.Range(0, TILE_SIZE - 2)
                    .Select(i => string.Concat(Enumerable.Range(0, TILE_SIZE - 2)
                        .Select(j => data[i, j]))
                    ).ToList();
            }

            

        }

        private enum Side
        {
            Top = 0,
            Right = 1,
            Bottom = 2,
            Left = 3
        }
    }
}
