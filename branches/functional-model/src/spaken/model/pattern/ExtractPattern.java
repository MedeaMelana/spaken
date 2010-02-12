package spaken.model.pattern;

import java.util.*;

import spaken.model.*;
import spaken.model.util.Unique;

public class ExtractPattern implements Spaken<ExtractPattern.Ref, NoException> {
	private List<Pos> inputs;

	private Map<Unique, Ref> knownPoints;

	private List<AxiomInvocation> construct;

	public ExtractPattern() {
		inputs = new ArrayList<Pos>();
		knownPoints = new HashMap<Unique, Ref>();
		construct = new LinkedList<AxiomInvocation>();
	}

	public Pattern makePattern() {
		return new Pattern(inputs.toArray(new Pos[inputs.size()]),
				construct.toArray(new AxiomInvocation[construct.size()]), null,
				inputs.size() + construct.size() - 1);
	}

	private Ref mkInput(Pos pos) {
		Ref ref = new Ref(-inputs.size()-1);
		inputs.add(pos);
		return ref;
	}

	private Ref doPush(AxiomInvocation ax) {
		Ref ref = new Ref(construct.size());
		construct.add(ax);
		return ref;
	}

	private Ref push(AxiomType type, int... r) {
		return doPush(new AxiomInvocation(type, r));
	}

	public Ref makePoint(Unique id, Pos pos) throws NoException {
		if (id == null) {
			return mkInput(pos);
		}

		Ref p = knownPoints.get(id);
		if (p == null) {
			p = mkInput(pos);
			knownPoints.put(id, p);
		}
		return p;
	}

	public Ref circle(Ref c, Ref f, Ref t) {
		return push(AxiomType.Circle, c.ref, f.ref, t.ref);
	}

	public Ref intersectCC(Ref c1, Ref c2) {
		return push(AxiomType.IntersectCC, c1.ref, c2.ref);
	}

	public Ref intersectLC(Ref l, Ref c) {
		return push(AxiomType.IntersectLC, l.ref, c.ref);
	}

	public Ref intersectLL(Ref l1, Ref l2) {
		return push(AxiomType.IntersectLL, l1.ref, l2.ref);
	}

	public Ref line(Ref p1, Ref p2) {
		return push(AxiomType.Line, p1.ref, p2.ref);
	}

	public Ref point1(Ref ps) throws NoException {
		return push(AxiomType.Point1, ps.ref);
	}

	public Ref point2(Ref ps) throws NoException {
		return push(AxiomType.Point2, ps.ref);
	}

	public Ref getPoint(Ref points, int i) throws NoException {
		if (i == 0) {
			return point1(points);
		} else if (i == 1) {
			return point2(points);
		} else {
			throw new PointIndexOutOfRangeException(i);
		}
	}

	private class Ref {
		private final int ref;

		private Ref(int ref) {
			this.ref = ref;
		}
	}

}
