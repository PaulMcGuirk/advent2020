using System;
namespace Advent.Text.Solvers
{
    /// <summary>
    /// Indicates that the class is a solver for a given problem
    /// </summary>
    [AttributeUsage(AttributeTargets.Class)]
    public class SolverAttribute : Attribute
    {
        /// <summary>
        /// The solver for this problem
        /// </summary>
        public string ProblemId { get; }

        /// <summary>
        /// Create a new instance
        /// </summary>
        /// <param name="problemId">The problem ID</param>
        public SolverAttribute(string problemId)
        {
            ProblemId = problemId;
        }
    }
}
