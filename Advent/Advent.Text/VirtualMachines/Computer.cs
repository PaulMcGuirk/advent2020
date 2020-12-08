using System.Collections.Generic;
using System.Linq;

namespace Advent.Text.VirtualMachines
{
    public class VirtualComputer
    {
        public List<Instruction> Instructions { get; set; }
        public int Accumulator { get; private set; }
        /// <summary>
        /// Current line number
        /// </summary>
        public int Position { get; private set; }

        public VirtualComputer(List<Instruction> instructions)
        {
            Instructions = instructions;
            Accumulator = 0;
            Position = 0;
        }

        /// <summary>
        /// Reset the accumulator and position
        /// </summary>
        public void Reset()
        {
            Accumulator = 0;
            Position = 0;
        }

        public bool IsTerminated => Position >= Instructions.Count;

        /// <summary>
        /// Execute the current line, and continue to the next one
        /// </summary>
        public void ExecuteCurrentLine()
        {
            var operation = Instructions[Position].Operation;
            var argument = Instructions[Position].Argument;

            _instructionDelegates[operation](this, argument);

            Position++;
        }

        /// <summary>
        /// Create a new solution from the given code
        /// </summary>
        /// <param name="s">The code to load</param>
        /// <returns>The virtual computer loaded with this code</returns>
        public static VirtualComputer Parse(string s)
        {
            var lines = s.Trim()
                .Split('\n')
                .Select(Instruction.Parse)
                .ToList();
            return new VirtualComputer(lines);
        }

        /// <summary>
        /// Delegates of this type will perform the logic of each operation
        /// </summary>
        /// <param name="computer">The computer the operation acts on</param>
        /// <param name="argument">The argument to the instruction</param>
        private delegate void DoInstruction(VirtualComputer computer, int argument);
        private Dictionary<Operation, DoInstruction> _instructionDelegates = new Dictionary<Operation, DoInstruction>
        {
            [Operation.Accumulate] = (computer, argument) => computer.Accumulator += argument,
            [Operation.Jump] = (computer, argument) => computer.Position += (argument - 1),
            [Operation.NoOperation] = (computer, argument) => { }
        };
    }

    /// <summary>
    /// Operations understood by this computer
    /// </summary>
    public enum Operation
    {
        /// <summary>
        /// Modify the accumlator
        /// </summary>
        Accumulate,
        /// <summary>
        /// Jump to an instruction
        /// </summary>
        Jump,
        /// <summary>
        /// Do nothing
        /// </summary>
        NoOperation
    }

    /// <summary>
    /// An instruction in the virtual computer
    /// </summary>
    public struct Instruction
    {
        public Operation Operation { get; private set; }
        public int Argument { get; private set; }

        public Instruction(Operation operation, int argument)
        {
            Operation = operation;
            Argument = argument;
        }

        /// <summary>
        /// Parse the instruction
        /// </summary>
        /// <param name="s">A string of the for [opName] [argument]</param>
        /// <returns>The parsed instruction</returns>
        public static Instruction Parse(string s)
        {
            var pieces = s.Split(' ');
            var type = _operationNames[pieces[0]];
            var argument = int.Parse(pieces[1]);

            return new Instruction(type, argument);
        }

        private readonly static Dictionary<string, Operation> _operationNames = new Dictionary<string, Operation>
        {
            ["acc"] = Operation.Accumulate,
            ["jmp"] = Operation.Jump,
            ["nop"] = Operation.NoOperation
        };
    }
}
