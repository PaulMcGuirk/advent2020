using System;
namespace Advent.Text.Solvers
{
    /// <summary>
    /// Solver for an advent of code 2020 problem
    /// </summary>
    public interface ISolver
    {
        /// <summary>
        /// Generate the solution
        /// </summary>
        /// <returns>The solution</returns>
        public object Solve();
    }
}
