using System;
using System.Reflection;
using System.Collections.Generic;
using Advent.Text.Solvers;
using System.IO;

namespace Advent
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length < 1)
            {
                Console.WriteLine("No problem number given");
                return;
            }

            var filepath = args.Length >= 2 ? args[1] : null;

            if (args[0] == "check")
            {
                if (string.IsNullOrEmpty(filepath))
                {
                    Console.WriteLine("No check file given");
                }
                DoCheck(filepath);
                return;
            }

            var problemId = args[0];

            Console.WriteLine($"Advent of Code 2020 Problem {problemId}");
            var result = GetSolution(problemId, filepath);

            if (result == null)
            {
                Console.WriteLine("No solution found");
            }

            Console.WriteLine($"{result}");
        }

        /// <summary>
        /// Get the solution for a single problem
        /// </summary>
        /// <param name="problemId">The problem ID</param>
        /// <param name="filepath">The path to the input file</param>
        /// <returns>The solution</returns>
        private static object GetSolution(string problemId, string filepath)
        {
            var solver = GetSolver(problemId);
            if (!string.IsNullOrEmpty(filepath))
            {
                solver.InputFilepath = filepath;
            }
            return solver.Solve();
        }

        /// <summary>
        /// Run the checks for a series of solved problems
        /// </summary>
        /// <param name="filepath">The filepath containing the check file</param>
        private static void DoCheck(string filepath)
        {
            var rawData = File.ReadAllText(filepath);
            var failedProblems = new List<string>();
            foreach (var line in rawData.Trim().Split('\n'))
            {
                var lineData = line.Split(' ');
                var problemId = lineData[0];
                var expected = lineData[1];
                var inputFilepath = lineData.Length >= 3 ? lineData[2] : null;
                if (!string.IsNullOrEmpty(inputFilepath))
                {
                    // this is pretty lousy
                    var lastSlashPos = filepath.LastIndexOf('/');
                    inputFilepath = filepath[..(lastSlashPos + 1)] + inputFilepath;
                }
                var actual = GetSolution(problemId, inputFilepath);
                var isMatch = actual.ToString() == expected;
                Console.WriteLine($"Problem: {problemId} Match: {isMatch} Expected: {expected} Actual: {actual}");
                if (!isMatch)
                {
                    failedProblems.Add(problemId);
                }
            }
            if (failedProblems.Count > 0)
            {
                Console.WriteLine($"Failed problems: {string.Join(", ", failedProblems)}");
            }
            else
            {
                Console.WriteLine("All problems passed");
            }
        }

        /// <summary>
        /// Get the solver for the problem ID
        /// </summary>
        /// <param name="problemId">The problem ID</param>
        /// <returns>The solver</returns>
        private static Solver GetSolver(string problemId)
        {
            foreach (var type in Assembly.GetExecutingAssembly().GetTypes())
            {
                if (type.IsInterface || type.IsAbstract)
                {
                    continue;
                }

                var interfaces = type.GetInterfaces();
                var isMatch = false;
                foreach (var interface_ in interfaces)
                {
                    if (interface_ == typeof(ISolver))
                    {
                        isMatch = true;
                        break;
                    }
                }

                if (!isMatch)
                {
                    continue;
                }

                if (type.GetCustomAttribute(typeof(SolverAttribute)) is SolverAttribute solverAttribute)
                {
                    if (solverAttribute.ProblemId == problemId)
                    {
                        return (Solver)Activator.CreateInstance(type);
                    }
                }
            }

            return null;
        }

        /// <summary>
        /// Get all of the solvers
        /// </summary>
        /// <returns></returns>
        private static IDictionary<string, Type> GetSolvers()
        {
            var solvers = new Dictionary<string, Type>();

            foreach (var type in Assembly.GetExecutingAssembly().GetTypes())
            {
                if (type.IsInterface || type.IsAbstract)
                {
                    continue;
                }

                var interfaces = type.GetInterfaces();
                var isMatch = false;
                foreach (var interface_ in interfaces)
                {
                    if (interface_ == typeof(ISolver))
                    {
                        isMatch = true;
                        break;
                    }
                }

                if (!isMatch)
                {
                    continue;
                }

                if (type.GetCustomAttribute(typeof(SolverAttribute)) is SolverAttribute solverAttribute)
                {
                    solvers[solverAttribute.ProblemId] = type;
                }
            }

            return solvers;
        }
    }
}
