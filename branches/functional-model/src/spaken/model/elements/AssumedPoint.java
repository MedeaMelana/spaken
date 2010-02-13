package spaken.model.elements;

import java.awt.Color;

import spaken.model.*;
import spaken.ui.swing.DrawingConstants;

public class AssumedPoint extends AbstractPoint {
	private Pos pos;
	
	AssumedPoint(Pos pos) {
		this.pos = pos;
	}
	
	public void setPointPos(Pos pos) {
		this.pos = pos;
	}
	
	public Pos getPointPos() {
		return pos;
	}

	public Pos getPos() throws ImaginaryPointException {
		return getPointPos();
	}

	public <Elem, Err extends Throwable> Elem visit(Spaken<Elem, Err> sp) throws Err {
		return sp.makePoint(getId(), pos);
	}
	
	protected Color getPointColor() {
		return DrawingConstants.CONTROLLABLE;
	}
	
}
