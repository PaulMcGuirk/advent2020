using System;
using System.Reflection;
using System.Collections.Generic;
using Advent.Text.Solvers;

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

            var problemId = args[0];
            var solver = GetSolver(problemId);

            if (args.Length >= 2)
            {
                solver.InputFilepath = args[1];
            }

            Console.WriteLine($"Advent of Code 2020 Problem {problemId}");

            var result = solver.Solve();

            if (result == null)
            {
                Console.WriteLine("No solution found");
            }

            Console.WriteLine($"{result}");
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
