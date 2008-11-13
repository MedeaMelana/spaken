package spaken.storage;

import java.io.IOException;

import spaken.model.Element;
import spaken.model.Pos;

public interface ElementWriter {
	public void writeRef(Element ref) throws IOException;
	public void writeRefs(Iterable<? extends Element> refs) throws IOException;

	public void writePos(Pos pos) throws IOException;
	
	public void writeDouble(double d) throws IOException;
}
