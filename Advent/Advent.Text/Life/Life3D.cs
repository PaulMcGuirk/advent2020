using System;
using System.Collections.Generic;
using System.Linq;

namespace Advent.Text.Life
{
    
    /// <summary>
    /// A generic game of 3D life in an infinite space with multiple states and
    /// rules that are specified by the consumer
    /// </summary>
    public class Life3D
    {
        private HashSet<Point> _alive;
        
        public Life3D()
        {
            _alive = new HashSet<Point>();
        }

        /// <summary>
        /// Set the initial state by specifying a finite plane. The rest of
        /// the space is taken to be empty
        /// </summary>
        /// <param name="state"></param>
        public void SetStatePlanar(string state)
        {
            _alive = new HashSet<Point>();

            var tiles = state.Trim().Split('\n').ToList();
            var numRows = tiles.Count;
            var numCols = tiles[0].Length;

            _alive = Enumerable.Range(0, numRows)
                .SelectMany(y => Enumerable.Range(0, numCols)
                                    .Where(x => tiles[y][x] == '#')
                                    .Select(x => new Point { X = x, Y = y }))
                .ToHashSet();
        }

        /// <summary>
        /// Simulate one tick in life.
        /// </summary>
        public void Tick()
            => _alive = _alive.SelectMany(tile => tile.GetNeighbors(true)).Where(WillSurvive).ToHashSet();

        /// <summary>
        /// Tick a given number of ticks
        /// </summary>
        /// <param name="numTicks">The number of ticks to tick</param>
        public void Tick(int numTicks)
        {
            for (var i = 0; i < numTicks; i++)
            {
                Tick();
            }
        }

        /// <summary>
        /// Determine whether a point will be alive in the next tick.
        /// </summary>
        /// <param name="point">The point to consider</param>
        /// <returns><c>true</c> if the point will be alive, false otherwise</returns>
        private bool WillSurvive(Point point)
        {
            var livingCount = point.GetNeighbors().Where(_alive.Contains).Count();

            return _alive.Contains(point) switch
            {
                true => livingCount == 2 || livingCount == 3,
                false => livingCount == 3
            };
        }

        /// <summary>
        /// Count the number of spaces that are alive
        /// </summary>
        /// <returns>The number of spaces currently alive</returns>
        public int CountLiving() => _alive.Count();

        /// <summary>
        /// A point in 3D space
        /// </summary>
        private record Point
        {
            public int X { get; init; }
            public int Y { get; init; }
            public int Z { get; init; }

            /// <summary>
            /// Get all of the neighbors of this point
            /// </summary>
            /// <param name="includeSelf">Whether to include the point itself in the return</param>
            /// <returns>The neighbors of this point</returns>
            public IEnumerable<Point> GetNeighbors(bool includeSelf = false)
            {
                for (var deltaZ = -1; deltaZ <= 1; deltaZ++)
                {
                    for (var deltaY = -1; deltaY <= 1; deltaY++)
                    {
                        for (var deltaX = -1; deltaX <= 1; deltaX++)
                        {
                            if (!includeSelf && deltaX == 0 && deltaY == 0 && deltaZ == 0)
                            {
                                continue;
                            }
                            var neighbor = new Point { X = X + deltaX, Y = Y + deltaY, Z = Z + deltaZ };
                            yield return neighbor;
                        }
                    }
                }
            }
        }
    }
}
