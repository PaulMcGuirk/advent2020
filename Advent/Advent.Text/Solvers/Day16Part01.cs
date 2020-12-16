using Advent.Text.TravelDocuments;

namespace Advent.Text.Solvers
{
    [Solver("16.1")]
    public class Day16Part01 : Solver
    { 

        public override object Solve()
        {
            var rawInput = ReadInputFile();

            var trainTicketData = TrainTicketData.Parse(rawInput);

            return trainTicketData.GetScanningErrorRate();
        }

    }
}
