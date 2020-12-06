using System.Collections.Generic;
using System.Linq;

namespace Advent.Text.Solvers
{
    [Solver("6.2")]
    public class Day06Part02 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();

            var result = contents.Trim()
                .Split("\n\n")
                .Select(group => CollectionIntersection(group.Split('\n')).Count())
                .Sum();

            return result;
        }

        /// <summary>
        /// Find the intersection of a collection of collections
        /// </summary>
        /// <typeparam name="T">The type of element in each collection</typeparam>
        /// <param name="collections">The collection of collections</param>
        /// <returns>The intersection</returns>
        private static IEnumerable<T> CollectionIntersection<T>(IEnumerable<IEnumerable<T>> collections)
        {
            if (!collections.Any())
            {
                return Enumerable.Empty<T>();
            }

            var enumerator = collections.GetEnumerator();
            enumerator.MoveNext();
            var result = enumerator.Current.ToHashSet();

            while (enumerator.MoveNext())
            {
                result.IntersectWith(enumerator.Current);
            }

            return result;
        }
    }
}
