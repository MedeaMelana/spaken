package spaken.storage;

import java.io.*;
import java.util.*;

import spaken.model.*;

public class FlatFileStorage implements StorageEngine {

	File file;

	public FlatFileStorage(File file) {
		this.file = file;
	}

	public void restoreSpace(Space space) {
		// TODO Auto-generated method stub

	}

	public void storeSpace(Space space) throws IOException {
		FlatElementWriter write = new FlatElementWriter();
		PrintWriter out = new PrintWriter(new FileWriter(file));

		for (Element e : space.getElements()) {
			write.writeElement(out, e);
		}

		out.close();
	}

	private class FlatElementWriter implements ElementWriter {

		private Map<Element, Integer> ids;

		private int nextID;

		private String type;

		private List<String> data;

		private FlatElementWriter() {
			ids = new HashMap<Element, Integer>();
			data = new ArrayList<String>(5); // 5 is probably always enough
			nextID = 0;
		}

		private int getID(Element e) {
			Integer id = ids.get(e);
			if (id == null) {
				id = nextID;
				ids.put(e, id);
				nextID++;
			}
			return id;
		}

		private void writeElement(PrintWriter out, Element elem)
				throws IOException {
			type = elem.getClass().getName();
			data.clear();
			
			try {
				elem.writeElement(this);
			} catch(IOException e) {
				throw e;
			} catch(Exception e) {
				throw new IOException(e.getMessage());
			}
			
			out.print(getID(elem));
			out.print(": ");
			out.print(type);
			for (String d : data) {
				out.print(" ");
				out.print(d);
			}
			out.println();
		}

		public void writePos(Pos pos) {
			data.add("(" + pos.x + "," + pos.y + ")");
		}

		public void writeRef(Element ref) {
			data.add("<" + getID(ref) + ">");
		}

		public void writeDouble(double d) throws IOException {
			data.add("d" + d);
		}

		public void writeRefs(Iterable<? extends Element> refs) throws IOException {
			data.add("[");
			for (Element ref : refs) {
				writeRef(ref);
			}
			data.add("]");
		}
	}

}
