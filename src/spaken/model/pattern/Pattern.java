package spaken.model.pattern;

import java.util.ArrayList;
import java.util.Arrays;

import spaken.model.*;
import spaken.util.Lookup;

public class Pattern {
	private Pos[] args;

	private AxiomInvocation[] construction;

	private ElemType returnType;

	private int returnRef;

	Pattern(Pos[] args, AxiomInvocation[] construction,
			ElemType returnType, int returnRef) {
		this.args = Arrays.copyOf(args, args.length);
		this.construction = Arrays.copyOf(construction, construction.length);
		this.returnType = returnType;
		this.returnRef = returnRef;
	}

	public <Elem, Err extends Throwable> Elem reconstruct(Spaken<Elem, Err> sp)
			throws Err {
		@SuppressWarnings("unchecked")
		Elem[] args = (Elem[]) new Object[this.args.length];
		for (int i = 0; i < args.length; i++) {
			args[i] = sp.makePoint(null, this.args[i]);
		}
		return instantiate(sp, args);
	}

	public <Elem, Err extends Throwable> Elem instantiate(Spaken<Elem, Err> sp,
			final Elem[] args) throws Err {
		if (args.length != this.args.length) {
			throw new IllegalArgumentException(
					"Wrong number of arguments supplied");
		}

		@SuppressWarnings("unchecked")
		final Elem[] stack = (Elem[]) new Object[args.length + construction.length];
		System.arraycopy(args, 0, stack, 0, args.length);
//		for (int i = 0; i < args.length; i++) {
//			stack[i] = args[args.length - i - 1];
//		}
		
		Lookup<Integer,Elem> lookup = new Lookup<Integer, Elem>() {
			public Elem get(Integer k) {
				if (k < 0) {
					return stack[args.length - Math.abs(k)];
				} else {
					return stack[k + args.length];
				}
			}
		};
		
		
		// Elem[] stack = (Elem[]) new Object[construction.length];
		for (int i = 0; i < construction.length; i++) {
			AxiomInvocation ax = construction[i];
			stack[i + args.length] = ax.invoke(sp, lookup);
		}

		return stack[returnRef];
	}

}
