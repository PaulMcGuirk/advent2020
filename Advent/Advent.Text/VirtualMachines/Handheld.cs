using System;
using System.Collections.Generic;

namespace Advent.Text.VirtualMachines
{
    public class Handheld
    {
        private VirtualComputer _computer;

        public Handheld(string bootCode)
        {
            _computer = VirtualComputer.Parse(bootCode);
        }

        /// <summary>
        /// Runs the VM, halting before a loop is identified
        /// </summary>
        /// <returns>The value of the accumulator immediately before
        /// the loop reexecutes</returns>
        public int IdentifyLoop()
        {
            var visited = new HashSet<int>();

            while (!visited.Contains(_computer.Position) && !_computer.IsTerminated)
            {
                visited.Add(_computer.Position);
                _computer.ExecuteCurrentLine();
            }

            return _computer.Accumulator;
        }

        /// <summary>
        /// Attempt to repair the loop in the boot code
        /// </summary>
        /// <returns>If the boot code is successfully loaded, the value
        /// of the accumulator at the end, or null if the code could not be repaired</returns>
        public int? RepairLoop()
        {
            for (var i = 0; i < _computer.Instructions.Count; i++)
            {
                _computer.Reset();

                var oldInstruction = _computer.Instructions[i];
                if (oldInstruction.Operation == Operation.Accumulate)
                {
                    continue;
                }

                var newOpertion = (oldInstruction.Operation == Operation.Jump) ? Operation.NoOperation : Operation.Jump;
                var newInsturction = new Instruction(newOpertion, oldInstruction.Argument);

                _computer.Instructions[i] = newInsturction;

                var val = IdentifyLoop();
                if (_computer.IsTerminated)
                {
                    return val;
                }

                _computer.Instructions[i] = oldInstruction;
            }
            return null;
        }
    }
}
