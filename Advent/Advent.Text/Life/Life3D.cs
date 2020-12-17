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
        private Point _max; // max an min will keep track of the extent of space we're worried about
        private Point _min;
        
        public Life3D()
        {
            _alive = new HashSet<Point>();
            _max = new Point { X = 0, Y = 0, Z = 0 };
            _min = new Point { X = 0, Y = 0, Z = 0 };
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
            _min = new Point { X = 0, Y = 0, Z = 0 };
            _max = new Point { X = tiles[0].Length, Y = tiles.Count, Z = 1 };

            _alive = Enumerable.Range(0, _max.Y)
                .SelectMany(y => Enumerable.Range(0, _max.X)
                                    .Where(x => tiles[y][x] == '#')
                                    .Select(x => new Point { X = x, Y = y, Z = 0 }))
                .ToHashSet();
        }

        /// <summary>
        /// Simulate one tick in life.
        /// </summary>
        public void Tick()
        {
            var newAlive = GetAllSpace().Where(WillSurvive).ToHashSet();
            _alive = newAlive;
            _min = new Point { X = _min.X - 1, Y = _min.Y - 1, Z = _min.Z - 1 };
            _max = new Point { X = _max.X + 1, Y = _max.Y + 1, Z = _max.Z + 1 };
        }

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
        public int CountLiving()
            => GetAllSpace().Count(_alive.Contains);

        /// <summary>
        /// Generate all relevant points in space
        /// </summary>
        /// <returns>The points to generate</returns>
        private IEnumerable<Point> GetAllSpace()
        {
            for (var z = _min.Z - 1; z <= _max.Z; z++)
            {
                for (var y = _min.Y - 1; y <= _max.Y; y++)
                {
                    for (var x = _min.X - 1; x <= _max.X; x++)
                    {
                        yield return new Point { X = x, Y = y, Z = z };
                    }
                }
            }
        }

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
            /// <returns>The neighbors of this point</returns>
            public IEnumerable<Point> GetNeighbors()
            {
                for (var deltaZ = -1; deltaZ <= 1; deltaZ++)
                {
                    for (var deltaY = -1; deltaY <= 1; deltaY++)
                    {
                        for (var deltaX = -1; deltaX <= 1; deltaX++)
                        {
                            if (deltaX == 0 && deltaY == 0 && deltaZ == 0)
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
