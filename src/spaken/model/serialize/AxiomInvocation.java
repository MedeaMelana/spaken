package spaken.model.serialize;

import java.util.Arrays;

import spaken.model.Spaken;
import spaken.model.util.Lookup;

public class AxiomInvocation {
	private final AxiomType type;

	private final int[] args;

	AxiomInvocation(AxiomType type, int[] args) {
		this.type = type;
		this.args = Arrays.copyOf(args, args.length);
	}

	public int[] getArgs() {
		return args;
	}

	public AxiomType getType() {
		return type;
	}

	// TODO Elem[] stack vervangen door een fatsoenlijke abstractie die het raar
	// indexeren (zie methode stack) zelf afhandelt.
	public <Elem, Err extends Throwable> Elem invoke(Spaken<Elem, Err> sp,
			Lookup<Integer,Elem> stack) throws Err {
		switch (type) {
		case Circle:
			return sp.circle(stack.get(args[0]), stack.get(args[1]),
					stack.get(args[2]));
		case IntersectCC:
			return sp.intersectCC(stack.get(args[0]), stack.get(args[1]));
		case IntersectLC:
			return sp.intersectLC(stack.get(args[0]), stack.get(args[1]));
		case IntersectLL:
			return sp.intersectLL(stack.get(args[0]), stack.get(args[1]));
		case Line:
			return sp.line(stack.get(args[0]), stack.get(args[1]));
		case Point1:
			return sp.getPoint(stack.get(args[0]), 0);
		case Point2:
			return sp.getPoint(stack.get(args[0]), 1);
		default:
			throw new RuntimeException("No type");
		}
	}
}
