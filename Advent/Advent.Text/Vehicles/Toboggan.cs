using System;
using System.Collections.Generic;
using System.Linq;

namespace Advent.Text.Vehicles
{
    public class Toboggan
    {

        /// <summary>
        /// The forest, expressed as a list of rows, where each row is a
        /// list of tiles.
        /// </summary>
        private readonly List<List<TileType>> _tiles;

        private readonly int _numRows;
        private readonly int _numCols;

        private Toboggan(List<List<TileType>> tiles)
        {
            _tiles = tiles;
            _numRows = _tiles.Count;
            _numCols = _tiles[0].Count;
        }

        /// <summary>
        /// Parse a tree from a string.
        /// </summary>
        /// <param name="s"></param>
        /// <returns></returns>
        public static Toboggan Parse(string s)
        {
            var tiles = s.Split('\n')
                .Where(line => !string.IsNullOrEmpty(line))
                .Select(ParseRow)
                .ToList();

            return new Toboggan(tiles);
        }

        private static List<TileType> ParseRow(string s)
            => s.Select(ParseTile).ToList();

        private static TileType ParseTile(char ch)
            => ch == '#' ? TileType.Tree : TileType.Free;

        /// <summary>
        /// Counts the number of tiles of a given type along a line from the
        /// top-left corner to the bottom row.
        /// </summary>
        /// <param name="tileType">The type of tile to count</param>
        /// <param name="slope">The slope of the line to take</param>
        /// <returns></returns>
        public int CountTileAlongLine(TileType tileType, Slope slope)
        {
            var result = 0;

            var (x, y) = (0, 0);

            while (y < _numRows)
            {
                var tile = _tiles[y][x];
                if (tile == tileType)
                {
                    result++;
                }

                y += slope.DeltaY;
                x += slope.DeltaX;
                x %= _numCols;
            }

            return result;
        }

    }

    public enum TileType
    {
        Free,
        Tree
    }

    /// <summary>
    /// Represents a slope
    /// </summary>
    public struct Slope
    {
        public int DeltaX { get; set; }
        public int DeltaY { get; set; }
    }
}
