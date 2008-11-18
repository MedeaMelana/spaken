package spaken.model.elements;

import java.io.IOException;
import java.util.Set;

import spaken.model.*;
import spaken.model.rendered.RenderedPoint;
import spaken.storage.ElementReader;
import spaken.storage.ElementWriter;

public class AssumedPoint extends AbstractPoint implements
		Comparable<AssumedPoint> {
	int index;

	/**
	 * Only used internally for reading and writing!
	 */
	public AssumedPoint() {
	}

	public AssumedPoint(int i) {
		this.index = i;
	}

	public int getIndex() {
		return index;
	}

	public void collectAssumptions(Set<AssumedPoint> collect) {
		collect.add(this);
	}

	public Point instantiate(PointBinding binding) throws UnboundPointException {
		return binding.getPoint(index);
	}

	public Pos getPos(PointBinding binding) throws ImaginaryPointException,
			UnboundPointException {
		return binding.getPoint(index).getPos(binding);
	}

	public RenderedPoint.Type getRenderedPointType() {
		return RenderedPoint.Type.PLUGGABLE;
	}

	public void writeElement(ElementWriter out) throws IOException {
		out.writeInt(index);
	}

	public void readElement(ElementReader in) throws IOException {
		index = in.readInt();
	}
	
	public boolean equals(Object that) {
		if (that == null) return false;
		if (that == this) return true;
		
		if (! (that instanceof AssumedPoint)) return false;
		
		return ((AssumedPoint) that).index == this.index;
	}
	
	public int hashCode() {
		return index;
	}

	public int compareTo(AssumedPoint that) {
		return Integer.valueOf(this.index).compareTo(
				Integer.valueOf(that.index));
	}

}
