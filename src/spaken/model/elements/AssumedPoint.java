package spaken.model.elements;

import java.util.Collection;

import spaken.model.*;
import spaken.util.Collector;

public class AssumedPoint extends AbstractPoint implements ElementListener<Point> {
	Point fallThrough;
	Pos pos;

	/**
	 * Only used internally for reading and writing!
	 */
	public AssumedPoint(Theorem theorem) {
		super(theorem);
	}
	
	public AssumedPoint(Theorem theorem, Pos pos) {
		super(theorem);
		fixAt(pos);
	}

	public AssumedPoint(Theorem theorem, Point point) {
		super(theorem);
		plug(point);
	}
	
	public void fixAt(Pos pos) {
		if (fallThrough != null) {
			fallThrough.removeElementListener(this);
		}

		this.pos = pos;
		this.fallThrough = null;
		
		notifyElementListeners(this);
	}
	
	public void plug(Point point) {
		this.pos = null;
		
		this.fallThrough = point;
		fallThrough.addElementListener(this);
		
		notifyElementListeners(this);
	}
	
	public boolean isFixed() {
		return (pos != null);
	}

	public void collectAssumptions(Collector<AssumedPoint> collect) {
		if (isFixed()) {
			collect.collect(this);
		} else {
			fallThrough.collectAssumptions(collect);
		}
	}

//	public Point instantiate(PointBinding<Point> binding)
//			throws UnboundPointException {
//		return binding.getBindingFor(index);
//	}

	public Pos getPos() throws ImaginaryPointException {
		if (isFixed()) {
			return pos;
		} else {
			return fallThrough.getPos();
		}
	}
	
	public void setPos(Pos pos) {
		fixAt(pos);
	}

	public Type getType() {
		if (isFixed()) {
			return Type.FIXED;
		} else {
			// TODO of iets anders ofzo
			return Type.DERIVED;
		}
	}

	public void collectDependencies(Collection<Element<?>> collect) {
		if (! isFixed()) {
			collect.add(fallThrough);
		}
	}

	public void elementChanged(Point e) {
		notifyElementListeners(this);
	}

//	public void writeElement(ElementWriter out) throws IOException {
//		out.writeInt(index);
//	}
//
//	public void readElement(ElementReader in) throws IOException {
//		index = in.readInt();
//	}

//	public boolean equals(Object that) {
//		if (that == null)
//			return false;
//		if (that == this)
//			return true;
//
//		if (!(that instanceof AssumedPoint))
//			return false;
//
//		return ((AssumedPoint) that).index == this.index;
//	}

//	public int hashCode() {
//		return index;
//	}

//	public int compareTo(AssumedPoint that) {
//		return Integer.valueOf(this.index).compareTo(
//				Integer.valueOf(that.index));
//	}

}
