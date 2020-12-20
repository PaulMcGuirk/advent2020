using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace Advent.Text.ImageProcessing
{
    public class JigsawCamera
    {
        private readonly Dictionary<long, Tile> _tiles;
        public int PictureSize { get; }
        private readonly Dictionary<string, List<long>> _allSides;

        private static readonly bool[] _boolVals = new bool[] { false, true };

        

        public JigsawCamera(string s)
        {
            _tiles = s.Trim().Split("\n\n")
                .Select(s => new Tile(s))
                .ToDictionary(tile => tile.Id);

            var numTiles = _tiles.Count;
            PictureSize = (int)Math.Sqrt(numTiles);
            if (PictureSize * PictureSize != numTiles)
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
                        var trial = new (long tileId, int numTurns, bool flipped)?[PictureSize, PictureSize];
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
                if (toAdd.col >= PictureSize)
                {
                    toAdd = (row: lastAdded.row + 1, col: 0);
                    // if we're trying to add below the picture, then that
                    // means that we're done
                    if (toAdd.row >= PictureSize)
                    {
                        var result = new (long tileId, int numTurns, bool isFlipped)[PictureSize, PictureSize];
                        for (var i = 0; i < PictureSize; i++)
                        {
                            for (var j = 0; j < PictureSize; j++)
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

            var imageSize = (Tile.TILE_SIZE - 2) * PictureSize;

            var pixels = new char[imageSize, imageSize];

            // patch the rotated and flipped tiles to form the over all image
            for (var i = 0; i < PictureSize; i++)
            {
                var tileData = Enumerable.Range(0, PictureSize)
                    .Select(j =>
                    {
                        var (tileId, numTurns, isFlipped) = solution[i, j];
                        var tile = _tiles[tileId];
                        return tile.GetData(numTurns, isFlipped);
                    })
                    .ToList();
                var numRows = tileData[0].Count;

                var rows = Enumerable.Range(0, numRows)
                    .Select(j => string.Concat(tileData.Select(x => x[j])))
                    .ToList();

                for (var k = 0; k < numRows; k++)
                {
                    for (var j = 0; j < imageSize; j++)
                    {
                        pixels[i * numRows + k, j] = rows[k][j];
                    }
                }
            }

            // transform the raw string into a mask that we'll use
            var pattern = patternString.Split('\n')
                .Select(s => s.Select((c, i) => (c, i)).Where(pair => pair.c == '#').Select(pair => pair.i).ToList())
                .ToList();

            return GetOrientations(pixels)
                .Select(p => GetRoughness(p, pattern))
                .FirstOrDefault(roughess => roughess.HasValue);
        }

        /// <summary>
        /// Get all possible rotations and flips of the square matrix of
        /// pixels
        /// </summary>
        /// <param name="pixels">A square matrix of pixels</param>
        /// <returns>All possible orientations</returns>
        private IEnumerable<char[,]> GetOrientations(char[,] pixels)
        {
            yield return pixels;

            for (var i = 0; i < 3; i++)
            {
                pixels = RotateSquareMatrix(pixels);
                yield return pixels;
            }

            pixels = RotateSquareMatrix(pixels);
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
        private static int? GetRoughness(char[,] pixels, List<List<int>> pattern)
        {
            var inPattern = new HashSet<(int, int)>();
            var patternLength = pattern.Select(l => l.Max()).Max();
            var size = pixels.GetLength(0);

            for (var i = 0; i <= (size - pattern.Count); i++)
            {
                for (var j = 0; j < size - patternLength; j++)
                {
                    var toCheck = new HashSet<(int row, int col)>();
                    for (var k = 0; k < pattern.Count; k++)
                    {
                        foreach (var pos in pattern[k])
                        {
                            toCheck.Add((i + k, j + pos));
                        }
                    }
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

            var result = 0;
            for (var i = 0; i < size; i++)
            {
                for (var j = 0; j < size; j++)
                {
                    if (inPattern.Contains((i, j)))
                    {
                        continue;
                    }
                    if (pixels[i, j] == '#')
                    {
                        result++;
                    }
                }
            }
            return result;
        }

        /// <summary>
        /// Get a flip of the square matrix
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
            public List<string> GetData(int numTurns, bool flipped)
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
