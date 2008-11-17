package spaken.storage;

import java.io.*;
import java.util.*;

import spaken.model.*;

public class FlatFileStorage implements StorageEngine {
	
	File file;

	public FlatFileStorage(File file) {
		this.file = file;
	}

	public void restoreSpace(Space space) throws IOException {
		BufferedReader in = new BufferedReader(new FileReader(file));
		FlatElementReader read = new FlatElementReader(in);
		
		read.readElements();
		
		for (FlatElementReader.LazyElement le : read.elements.values()) {
			if (! le.isHidden()) {
				space.add(le.getElement());
			}
		}
		
		in.close();
	}
	
	private class FlatElementReader {

		private BufferedReader in;
		private Map<String, LazyElement> elements;

		private FlatElementReader(BufferedReader in) {
			this.in = in;
			elements = new HashMap<String, LazyElement>();
		}

		private void readElements() throws IOException {
			elements.clear();
			String line;
			while ((line = in.readLine()) != null) {
				line = line.trim();
				if (! line.equals("") && ! line.startsWith("#")) {
					String[] parts = line.split(":", 2);
					String id = parts[0].trim();
					
					boolean hidden = false;
					if (id.startsWith("(") && id.endsWith(")")) {
						hidden = true;
						id = id.substring(1, id.length() - 1);
					}
					
					elements.put(id, new LazyElement(parts[1].trim(), hidden));
				}
			}
		}
		
		private class LazyElement implements ElementReader {
			LinkedList<String> words;
			Element element;
			boolean hidden;
			
			private LazyElement(String str, boolean hidden) {
				words = new LinkedList<String>();
				Collections.addAll(words, str.split("\\s+"));
				this.hidden = hidden;
			}
			
			private boolean isHidden() {
				return hidden;
			}
			
			private Element getElement() throws IOException {
				if (element == null) {
					String type = nextWord();
					try {
						@SuppressWarnings("unchecked")
						Class<Element> clazz = (Class<Element>) Class.forName(type);
						
						element = clazz.newInstance();
						element.readElement(this);
					} catch (Exception e) {
						throw new IOException(e.toString());
					}
					
				}
				
				return element;
			}
			
			private String nextWord() throws IOException {
				String next = words.poll();
				
				if (next == null) {
					throw new IOException("Words depleted");
				}
				
				return next;
			}
			
			public boolean readBoolean() throws IOException {
				return Boolean.parseBoolean(nextWord());
			}
			
			public double readDouble() throws IOException {
				return Double.parseDouble(nextWord());
			}
			
			public Pos readPos() throws IOException {
				return Pos.parsePos(nextWord());
			}
			
			public Element readRef() throws IOException {
				return readRef(nextWord());
			}
			
			private Element readRef(String id) throws IOException {
				LazyElement le = elements.get(id);
				if (le == null) {
					throw new IOException("Undefined element '" + id + "'");
				}
				return le.getElement();
			}
			
			public List<Element<?>> readRefs() throws IOException {
				if (!nextWord().equals("[")) {
					throw new IOException("Expected '['");
				}
				
				List<Element<?>> refs = new LinkedList<Element<?>>();
				String word = nextWord();
				while (! word.equals("]")) {
					refs.add(readRef(word));
					word = nextWord();
				}
				
				return refs;
			}
		}
	}

	public void storeSpace(Space space) throws IOException {
		PrintWriter out = new PrintWriter(new FileWriter(file));
		FlatElementWriter write = new FlatElementWriter(out);

		Set<Element> unsaved = new HashSet<Element>(space.getElements());
		
		boolean hidden = false;
		
		while (! unsaved.isEmpty()) {
			for (Element e : unsaved) {
				write.writeElement(e, hidden);
			}
			
			hidden = true;
			unsaved.clear();
			unsaved.addAll(write.unsavedRefs);
		}
		
		out.close();
	}

	private class FlatElementWriter implements ElementWriter {

		private PrintWriter out;
		
		private Map<Element, Integer> ids;
		
		private int nextID;

		private Set<Element> unsavedRefs;

		private FlatElementWriter(PrintWriter out) {
			this.out = out;
			ids = new HashMap<Element, Integer>();
			// data = new ArrayList<String>(5); // 5 is probably always enough
			unsavedRefs = new HashSet<Element>();
			nextID = 0;
		}

		private String getID(Element e) {
			Integer id = ids.get(e);
			if (id == null) {
				id = nextID;
				ids.put(e, id);
				nextID++;
			}
			return "e" + id;
		}
		
		private void writeElement(Element elem, boolean hidden)
				throws IOException {
			String type = elem.getClass().getName();
			
			if (hidden)	out.print("(");
			out.print(getID(elem));
			if (hidden) out.print(")");
			
			out.print(": ");
			out.print(type);
			
			try {
				elem.writeElement(this);
			} catch(IOException e) {
				throw e;
			} catch(Exception e) {
				throw new IOException(e.getMessage());
			}
			
			out.println();
			
			unsavedRefs.remove(elem);
		}

		public void writePos(Pos pos) {
			out.print(" ");
			out.print(pos);
		}

		public void writeRef(Element ref) {
			if (! ids.containsKey(ref)) {
				unsavedRefs.add(ref);
			}
			out.print(" ");
			out.print(getID(ref));
		}

		public void writeDouble(double d) throws IOException {
			out.print(" ");
			out.print(d);
		}
		
		public void writeBoolean(boolean b) throws IOException {
			out.print(" ");
			out.print(b);
		}

		public void writeRefs(Iterable<? extends Element<?>> refs) throws IOException {
			out.print(" [");
			for (Element ref : refs) {
				writeRef(ref);
			}
			out.print(" ]");
		}
	}

}
