package spaken.model.elements;

import java.util.Set;

import spaken.model.Pos;

public class AssumedPoint extends AbstractPoint { //implements
		//Comparable<AssumedPoint> {
	Pos pos;

	/**
	 * Only used internally for reading and writing!
	 */
	public AssumedPoint() {
	}
	
	public AssumedPoint(Pos pos) {
		this.pos = pos;
	}

	public void collectAssumptions(Set<AssumedPoint> collect) {
		collect.add(this);
	}

//	public Point instantiate(PointBinding<Point> binding)
//			throws UnboundPointException {
//		return binding.getBindingFor(index);
//	}

	public Pos getPos() {
		return pos;
	}
	
	public void setPos(Pos pos) {
		this.pos = pos;
	}

	public Type getType() {
		return Type.FIXED;
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
