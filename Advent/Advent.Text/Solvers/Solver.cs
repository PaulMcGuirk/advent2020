using System;
using System.IO;

namespace Advent.Text.Solvers
{
    /// <summary>
    /// Solves an Advent of Code problem
    /// </summary>
    public abstract class Solver : ISolver
    {
        /// <summary>
        /// Run the solver to get the solution.
        /// </summary>
        /// <returns><The solution/returns>
        public abstract object Solve();

        /// <summary>
        /// The filepath to the solution
        /// </summary>
        public string InputFilepath { get; set; }

        /// <summary>
        /// Load the input file into memory.
        /// </summary>
        /// <returns></returns>
        protected string ReadInputFile()
        {
            if (string.IsNullOrEmpty(InputFilepath))
            {
                Console.WriteLine("Expected filepath");
                return string.Empty;
            }

            try
            {
                var rawData = File.ReadAllText(InputFilepath);
                return rawData;
            }
            catch (Exception e)
            {
                Console.WriteLine("Failed to read input file.");
                Console.WriteLine(e.Message);
                return string.Empty;
            }
        }
    }
}
