using System.Linq;

namespace Advent.Text.Solvers
{
    [Solver("13.1")]
    public class Day13Part01 : Solver
    {
        public override object Solve()
        {
            var rawInput = ReadInputFile();

            var lines = rawInput.Trim().Split('\n');
            var earliestDepartTime = int.Parse(lines[0]);
            var busIds = lines[1].Split(',')
                .Where(piece => piece != "x")
                .Select(int.Parse);

            var earliest = busIds
                .Select(busId => (busId, waitTime: WaitTime(earliestDepartTime, busId)))
                .Aggregate((min, next) => next.waitTime < min.waitTime ? next : min);

            return earliest.busId * earliest.waitTime;
        }

        /// <summary>
        /// Get the amount of time to wait for a particular bus
        /// </summary>
        /// <param name="earliestDepartTime">The earliest you want to leave</param>
        /// <param name="busId">The bus ID to wait for</param>
        /// <returns>The amount of time you need to wait</returns>
        private int WaitTime(int earliestDepartTime, int busId)
        {
            var remainder = earliestDepartTime % busId;

            return remainder == 0 ? 0 : busId - remainder;
        }
    }
}
