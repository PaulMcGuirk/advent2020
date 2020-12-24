using System;
using System.Collections.Generic;
using System.Linq;

namespace Advent.Text.Life
{
    public class HexagonalLife
    {

        private HashSet<HexagonalPoint> _tiles;
        private static readonly HexagonalPoint _origin = new HexagonalPoint { Q = 0, R = 0 };

        public HexagonalLife()
        {
            _tiles = new HashSet<HexagonalPoint>();
        }

        /// <summary>
        /// Set the initial state
        /// </summary>
        /// <param name="instructionStrings">Strings describing each position to toggle</param>
        public void SetInitialState(IEnumerable<string> instructionStrings)
        {
            foreach (var instructionString in instructionStrings)
            {
                var flipInstruction = ParseInstructionString(instructionString);
                FlipTile(flipInstruction);
            }
        }

        /// <summary>
        /// Flip a tile based on a path from the origin
        /// </summary>
        /// <param name="directions">A path from the origin to the point
        /// that will be flipped</param>
        private void FlipTile(IEnumerable<Directions> directions)
        {
            var tile = _origin;
            foreach (var direction in directions)
            {
                tile = tile.GetNeighbor(direction);
            }

            if (_tiles.Contains(tile))
            {
                _tiles.Remove(tile);
            }
            else
            {
                _tiles.Add(tile);
            }
        }

        /// <summary>
        /// Count the number of active tiles
        /// </summary>
        /// <returns></returns>
        public int CountActiveTiles() => _tiles.Count();

        /// <summary>
        /// Run for a given number of ticks
        /// </summary>
        /// <param name="numTicks">The number of ticks to run for</param>
        public void Run(int numTicks)
        {
            for (var i = 0; i < numTicks; i++)
            {
                Tick();
            }
        }

        /// <summary>
        /// Tick by one unit
        /// </summary>
        public void Tick()
        {
            var toCheck = _tiles.Union(_tiles.SelectMany(tile => tile.GetNeighbors())).ToList();

            _tiles = toCheck.Where(tile =>
            {
                var activeNeighbors = tile.GetNeighbors().Count(neighor => _tiles.Contains(neighor));
                if (_tiles.Contains(tile))
                {
                    return activeNeighbors == 1 || activeNeighbors == 2;
                }
                else
                {
                    return activeNeighbors == 2;
                }
            }).ToHashSet();
        }

        /// <summary>
        /// Parse an instruction string into a list of directions
        /// </summary>
        /// <param name="s">The string to arse</param>
        /// <returns>A list of directions</returns>
        private static List<Directions> ParseInstructionString(string s)
        {
            var directions = new List<Directions>();

            var prevCh = 0;
            foreach (var ch in s)
            {
                if (ch == 'e')
                { 
                    if (prevCh == 'n')
                    {
                        directions.Add(Directions.NorthEast);
                    }
                    else if (prevCh == 's')
                    {
                        directions.Add(Directions.SouthEast);
                    }
                    else
                    {
                        directions.Add(Directions.East);
                    }
                }
                else if (ch == 'w')
                {
                    if (prevCh == 'n')
                    {
                        directions.Add(Directions.NorthWest);
                    }
                    else if (prevCh == 's')
                    {
                        directions.Add(Directions.SouthWest);
                    }
                    else
                    {
                        directions.Add(Directions.West);
                    }
                }
                prevCh = ch;
            }

            return directions;
        }

        /// <summary>
        /// A point on a hexagonal grid, using an axial system
        /// </summary>
        private record HexagonalPoint
        {
            public int Q { get; init; }
            public int R { get; init; }

            public HexagonalPoint GetNeighbor(Directions direction)
            {
                var (newQ, newR) = direction switch
                {
                    Directions.East => (Q + 1, R),
                    Directions.SouthEast => (Q, R + 1),
                    Directions.SouthWest => (Q - 1, R + 1),
                    Directions.West => (Q - 1, R),
                    Directions.NorthWest => (Q, R - 1),
                    Directions.NorthEast => (Q + 1, R - 1),
                    _ => throw new ArgumentException($"Unrecognized direction: {direction}")
                };

                return new HexagonalPoint { Q = newQ, R = newR };
            }

            /// <summary>
            /// Get all of the neighrbors of a point
            /// </summary>
            /// <returns>All of the neighbors</returns>
            public IEnumerable<HexagonalPoint> GetNeighbors()
            {
                foreach (var dir in Enum.GetValues(typeof(Directions)))
                {
                    yield return GetNeighbor((Directions)dir);
                }
            }
        }

        private enum Directions
        {
            East,
            SouthEast,
            SouthWest,
            West,
            NorthWest,
            NorthEast
        }
    }
}
