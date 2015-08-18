/* part of flags 
 */
		case 'B':
			valueScale = VS_BOOLEAN;
			break;
		case 'L':
			valueScale = VS_LDD;
			break;
		case 'N':
			valueScale = VS_NOMINAL;
			break;
		case 'O':
			valueScale = VS_ORDINAL;
			break;
		case 'S':
			valueScale = VS_SCALAR;
			break;
		case 'D':
			valueScale = VS_DIRECTION;
			break;
		case 'V':
			Error("Vector not yet implemented");
			break;
