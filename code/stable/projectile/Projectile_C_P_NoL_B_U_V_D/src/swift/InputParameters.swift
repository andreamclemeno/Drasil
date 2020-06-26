/** InputParameters.swift
    Provides the structure for holding input values, the function for reading inputs, and the function for checking the physical constraints on the input
    - Authors: Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
*/
import Foundation

extension String: Error {}

/** Structure for holding the input values
*/
class InputParameters {
    var v_launch: Double = 0.0
    var theta: Double = 0.0
    var p_target: Double = 0.0
    
    /** Initializes input object by reading inputs and checking physical constraints on the input
        - Parameter filename: name of the input file
    */
    init(_ filename: String) {
        try self.get_input(filename)
        self.input_constraints()
    }
    
    /** Reads input from a file with the given file name
        - Parameter filename: name of the input file
    */
    private func get_input(_ filename: String) throws -> Void {
        var infile: URL
        infile = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent(filename)
        var contents: [[String]]
        do {
            contents = try String(contentsOf: infile).components(separatedBy: "\n").map({(l: String) -> [String] in l.components(separatedBy: " ")})
        } catch {
            throw "Error reading from file."
        }
        self.v_launch = Double(contents[1][0])!
        self.theta = Double(contents[2][0])!
        self.p_target = Double(contents[3][0])!
    }
    
    /** Verifies that input values satisfy the physical constraints
    */
    private func input_constraints() -> Void {
        if !(self.v_launch > 0) {
            print("Warning: ", terminator: "")
            print("v_launch has value ", terminator: "")
            print(self.v_launch, terminator: "")
            print(", but is suggested to be ", terminator: "")
            print("above ", terminator: "")
            print(0, terminator: "")
            print(".")
        }
        if !(0 < self.theta && self.theta < Double.pi / 2) {
            print("Warning: ", terminator: "")
            print("theta has value ", terminator: "")
            print(self.theta, terminator: "")
            print(", but is suggested to be ", terminator: "")
            print("between ", terminator: "")
            print(0, terminator: "")
            print(" and ", terminator: "")
            print(Double.pi / 2, terminator: "")
            print(" ((pi)/(2))", terminator: "")
            print(".")
        }
        if !(self.p_target > 0) {
            print("Warning: ", terminator: "")
            print("p_target has value ", terminator: "")
            print(self.p_target, terminator: "")
            print(", but is suggested to be ", terminator: "")
            print("above ", terminator: "")
            print(0, terminator: "")
            print(".")
        }
    }
}
