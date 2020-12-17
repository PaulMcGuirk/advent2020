using System;
using System.Collections.Generic;
using System.Linq;

namespace Advent.Text.Life
{

    /// <summary>
    /// A generic game of 3D life in an infinite space with multiple states and
    /// rules that are specified by the consumer
    /// </summary>
    public class LifeND<TPoint> where TPoint: IPointND, new()
    {
        // will contain all living tiles and all neighbors of living tiles
        // these are the tiles that may change state at the next tick
        private Dictionary<TPoint, bool> _state;

        public LifeND()
        {
            _state = new Dictionary<TPoint, bool>();
        }

        /// <summary>
        /// Set the initial state by specifying a finite plane. The rest of
        /// the space is taken to be empty
        /// </summary>
        /// <param name="state"></param>
        public void SetStatePlanar(string state)
        {
            var tiles = state.Trim().Split('\n').ToList();
            var numRows = tiles.Count;
            var numCols = tiles[0].Length;

            // add in all living tiles
            _state = Enumerable.Range(0, numRows)
                .SelectMany(y => Enumerable.Range(0, numCols)
                                    .Where(x => tiles[y][x] == '#')
                                    .Select(x => new TPoint { X = x, Y = y }))
                .ToDictionary(point => point, point => true);

            // add the neighbors of living tiles
            var neighbors = _state.Keys.SelectMany(point => point.GetNeighbors(false))
                .Select(neighbor => (TPoint)neighbor)
                .ToList();

            foreach (var neighbor in neighbors)
            {
                if (!_state.ContainsKey(neighbor))
                {
                    _state[neighbor] = false;
                }
            }
        }

        /// <summary>
        /// Simulate one tick in life.
        /// </summary>
        public void Tick()
        {
            var newStatePoints = new HashSet<TPoint>();
            var newAlive = new HashSet<TPoint>();

            foreach (var point in _state.Keys)
            {
                var pointNeighbors = point.GetNeighbors(false)
                    .Select(neighbor => (TPoint)neighbor)
                    .ToHashSet();

                var livingCount = pointNeighbors.Where(_state.GetValueOrDefault).Count();

                if (Survives(_state.GetValueOrDefault(point), livingCount))
                {
                    newAlive.Add(point);
                    newStatePoints.UnionWith(pointNeighbors);
                    newStatePoints.Add(point);
                }
            }

            _state = newStatePoints.ToDictionary(point => point, point => newAlive.Contains(point));
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
        /// Determine whether or not the givent tile survives
        /// </summary>
        /// <param name="isAlive">The current state</param>
        /// <param name="livingNeighborCount">The number of living neighbors</param>
        /// <returns><c>true</c> if it will survive, false otherwise</returns>
        private static bool Survives(bool isAlive, int livingNeighborCount)
            => livingNeighborCount == 3 || (livingNeighborCount == 2 && isAlive);

        /// <summary>
        /// Count the number of spaces that are alive
        /// </summary>
        /// <returns>The number of spaces currently alive</returns>
        public int CountLiving() => _state.Values.Count(v => v);

    }

    /// <summary>
    /// A point in at least 2 dimensional space
    /// </summary>
    public interface IPointND
    {
        public int X { get; init; }
        public int Y { get; init; }

        /// <summary>
        /// Generates all neighbors of the point
        /// </summary>
        /// <param name="includeSelf">Whether to include a copy of the point itself
        /// among the neighbors</param>
        /// <returns>The neighbors</returns>
        public IEnumerable<IPointND> GetNeighbors(bool includeSelf);
    }

    /// <summary>
    /// A point in 3D space
    /// </summary>
    public record Point3D : IPointND
    {
        public int X { get; init; }
        public int Y { get; init; }
        public int Z { get; init; }

        public virtual IEnumerable<Point3D> GetNeighbors(bool includeSelf)
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
                        yield return new Point3D { X = X + deltaX, Y = Y + deltaY, Z = Z + deltaZ };
                    }
                }
            }
        }

        IEnumerable<IPointND> IPointND.GetNeighbors(bool includeSelf) => GetNeighbors(includeSelf);
    }

    /// <summary>
    /// A point in 4D space
    /// </summary>
    public record Point4D : Point3D
    {
        public int W { get; init; }

        public override IEnumerable<Point4D> GetNeighbors(bool includeSelf)
        {
            var point3D = new Point3D { X = X, Y = Y, Z = Z };
            foreach (var neighbor3D in point3D.GetNeighbors(true))
            {
                for (var deltaW = -1; deltaW <= 1; deltaW++)
                {
                    if (!includeSelf && deltaW == 0 && neighbor3D.X == X && neighbor3D.Y == Y && neighbor3D.Z == Z)
                    {
                        continue;
                    }
                    yield return new Point4D { X = neighbor3D.X, Y = neighbor3D.Y, Z = neighbor3D.Z, W = W + deltaW };
                }
            }
        }
    }
}
