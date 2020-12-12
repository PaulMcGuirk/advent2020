using System;
using System.Collections.Generic;
using System.Linq;

namespace Advent.Text.Vehicles
{
    public class Ferry
    {
        public enum NavigationModes
        {
            Absolute,
            WayPoint
        }

        /// <summary>
        /// The direction that the ferry is currently facing
        /// </summary>
        public Directions Direction { get; private set; }

        /// <summary>
        /// The current position of the ferry
        /// </summary>
        public Vector Position { get; private set; }

        /// <summary>
        /// The current waypoint
        /// </summary>
        public Vector WayPoint { get; private set; }

        public NavigationModes NavigationMode { get; }
        
        /// <summary>
        /// Directions that the ship can face
        /// </summary>
        public enum Directions
        {
            North = 0,
            East = 1,
            South = 2,
            West = 3
        }

        /// <summary>
        /// An integer vector
        /// </summary>
        public struct Vector
        {
            public int X { get; set; }
            public int Y { get; set; }

            public static Vector operator *(int c, Vector v) => new Vector { X = c * v.X, Y = c * v.Y };
            public static Vector operator +(Vector v1, Vector v2) => new Vector { X = v1.X + v2.X, Y = v1.Y + v2.Y };

            public int Magnitude => Math.Abs(X) + Math.Abs(Y);

            /// <summary>
            /// Rotate a vector left a multiple of 90 degrees
            /// </summary>
            /// <param name="numTurns">The number of multiples</param>
            /// <returns>The rotated vector</returns>
            public Vector RotateLeft(int numTurns)
            {
                var x = X;
                var y = Y;
                for (var i = 0; i < numTurns; i++)
                {
                    (x, y) = (y, -x);
                }
                return new Vector { X = x, Y = y };
            }

            /// <summary>
            /// Rotate a vector right a multiple of 90 degrees
            /// </summary>
            /// <param name="numTurns">The number of multiples</param>
            /// <returns>The rotated vector</returns>
            public Vector RotateRight(int numTurns)
            {
                var x = X;
                var y = Y;
                for (var i = 0; i < numTurns; i++)
                {
                    (x, y) = (-y, x);
                }
                return new Vector { X = x, Y = y };
            }
        }

        public Ferry(Directions initialDirection,
            NavigationModes navigationMode,
            Vector? InitialWaypoint = null)
        {
            Position = new Vector { X = 0, Y = 0 };
            Direction = initialDirection;
            NavigationMode = navigationMode;

            WayPoint = InitialWaypoint ?? new Vector();
        }

        /// <summary>
        /// Navigate per the given string of instructions
        /// </summary>
        public void Navigate(string instructionString)
        {
            instructionString.Trim()
                .Split('\n')
                .Select(Instruction.Parse)
                .ToList()
                .ForEach(Navigate);
        }

        private void Navigate(Instruction instruction)
        {
            var del = NavigationMode == NavigationModes.Absolute
                ? _navigationHandlers[instruction.Type]
                : _navigationWaypointHandlers[instruction.Type];

            del(this, instruction.Argument);
        }

        private delegate void NavigationHandler(Ferry ferry, int argument);
        private Dictionary<InstructionTypes, NavigationHandler> _navigationHandlers = new Dictionary<InstructionTypes, NavigationHandler>
        {
            [InstructionTypes.North] = (ferry, argument) => ferry.Position += argument * _unitVectors[Directions.North],
            [InstructionTypes.East] = (ferry, argument) => ferry.Position += argument * _unitVectors[Directions.East],
            [InstructionTypes.South] = (ferry, argument) => ferry.Position += argument * _unitVectors[Directions.South],
            [InstructionTypes.West] = (ferry, argument) => ferry.Position += argument * _unitVectors[Directions.West],
            [InstructionTypes.Left] = (ferry, argument) => ferry.Direction = (Directions)(((int)ferry.Direction - argument / 90 + 4) % 4),
            [InstructionTypes.Right] = (ferry, argument) => ferry.Direction = (Directions)(((int)ferry.Direction + argument / 90) % 4),
            [InstructionTypes.Forward] = (ferry, argument) => ferry.Position += argument * _unitVectors[ferry.Direction]
        };

        private Dictionary<InstructionTypes, NavigationHandler> _navigationWaypointHandlers = new Dictionary<InstructionTypes, NavigationHandler>
        {
            [InstructionTypes.North] = (ferry, argument) => ferry.WayPoint += argument * _unitVectors[Directions.North],
            [InstructionTypes.East] = (ferry, argument) => ferry.WayPoint += argument * _unitVectors[Directions.East],
            [InstructionTypes.South] = (ferry, argument) => ferry.WayPoint += argument * _unitVectors[Directions.South],
            [InstructionTypes.West] = (ferry, argument) => ferry.WayPoint += argument * _unitVectors[Directions.West],
            [InstructionTypes.Left] = (ferry, argument) => ferry.WayPoint = ferry.WayPoint.RotateLeft(argument / 90),
            [InstructionTypes.Right] = (ferry, argument) => ferry.WayPoint = ferry.WayPoint.RotateRight(argument / 90),
            [InstructionTypes.Forward] = (ferry, argument) => ferry.Position += argument * ferry.WayPoint
        };

        private static readonly Dictionary<Directions, Vector> _unitVectors = new Dictionary<Directions, Vector>
        {
            [Directions.North] = new Vector { X = 0, Y = -1 },
            [Directions.East] = new Vector { X = 1, Y = 0 },
            [Directions.South] = new Vector { X = 0, Y = 1 },
            [Directions.West] = new Vector { X = -1, Y = 0 }
        };

        /// <summary>
        /// Types of instructions that cna be used to navigate the ship
        /// </summary>
        private enum InstructionTypes
        {
            North,
            East,
            South,
            West,
            Left,
            Right,
            Forward
        }

        /// <summary>
        /// An instruction for the ferry
        /// </summary>
        private class Instruction
        {
            public InstructionTypes Type { get; set; }
            public int Argument { get; set; }

            public static Instruction Parse(string s)
            {
                var instructionChar = s[0];
                var argumentString = s[1..];

                return new Instruction
                {
                    Type = _instructionChars[instructionChar],
                    Argument = int.Parse(argumentString)
                };
            }

            private static Dictionary<char, InstructionTypes> _instructionChars = new Dictionary<char, InstructionTypes>
            {
                ['N'] = InstructionTypes.North,
                ['E'] = InstructionTypes.East,
                ['S'] = InstructionTypes.South,
                ['W'] = InstructionTypes.West,
                ['L'] = InstructionTypes.Left,
                ['R'] = InstructionTypes.Right,
                ['F'] = InstructionTypes.Forward
            };
        }
    }
}
