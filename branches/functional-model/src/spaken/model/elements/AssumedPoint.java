package spaken.model.elements;

import spaken.model.*;
import spaken.model.util.Unique;

public class AssumedPoint extends AbstractElement implements Point {
	private final Unique id;
	private final Pos pos;
	
	AssumedPoint(Pos pos) {
		id = Unique.create();
		this.pos = pos;
	}

	public Pos getPos() throws ImaginaryPointException {
		return pos;
	}

	public <Elem, Err extends Throwable> Elem visit(Spaken<Elem, Err> sp) throws Err {
		return sp.makePoint(id, pos);
	}
	
}
