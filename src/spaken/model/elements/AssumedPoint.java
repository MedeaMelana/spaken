package spaken.model.elements;

import java.io.IOException;

import spaken.model.*;
import spaken.model.rendered.RenderedPoint;
import spaken.storage.ElementReader;
import spaken.storage.ElementWriter;

public class AssumedPoint extends AbstractPoint {
	/*
	 * An AssumedPoint is either assumed from thin air (with an example Pos) or
	 * filled in with an actual Point.
	 */
	private boolean isActual;
	private Point actual;
	private Pos example;

	public AssumedPoint(Point actual) {
		setActual(actual);
	}
	
	public AssumedPoint(Pos example) {
		setExample(example);
	}

	public boolean isActual() {
		return isActual;
	}

	public void setExample(Pos example) {
		isActual = false;
		this.example = example;
	}

	public void setActual(Point actual) {
		isActual = true;
		this.actual = actual;
	}
	
	public Pos getExample() {
		if (isActual) {
			throw new IllegalStateException("This AssumedPoint does not have an example");
		} else {
			return example;
		}
	}

	public Point getActual() {
		if (isActual) {
			return actual;
		} else {
			throw new IllegalStateException("This AssumedPoint does not have an actual point");
		}
	}

	public Pos getPos() throws ImaginaryPointException {
		if (isActual) {
			return actual.getPos();
		} else {
			return example;
		}
	}
	
	// compatibility with FixedPoint
	public void setPos(Pos pos) {
		//TODO OF! proberen de positie van actual te veranderen als isActual
		setExample(pos);
	}

	public RenderedPoint.Type getRenderedPointType() {
		if (isActual) {
			return RenderedPoint.Type.PLUGGABLE;
		} else {
			return RenderedPoint.Type.FIXED;
		}
	}

	public void writeElement(ElementWriter out) throws IOException {
		// TODO needs more thought
		
		out.writeBoolean(isActual);
		if (isActual) {
			out.writeRef(actual);
		} else {
			out.writePos(example);
		}
	}

	public void readElement(ElementReader in) throws IOException {
		// TODO needs more thought
		
		isActual = in.readBoolean();
		if (isActual) {
			actual = (Point) in.readRef();
		} else {
			example = in.readPos();
		}
	}
}
