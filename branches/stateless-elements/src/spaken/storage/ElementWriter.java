package spaken.storage;

import java.io.IOException;

import spaken.model.Element;
import spaken.model.Pos;

public interface ElementWriter {
	// TODO some kind of ParseError instead of IOException

	public void writeRef(Element ref) throws IOException;
	public void writeRefs(Iterable<? extends Element<?>> refs) throws IOException;

	public void writePos(Pos pos) throws IOException;
	
	public void writeDouble(double d) throws IOException;
	public void writeBoolean(boolean isActual) throws IOException;
}
